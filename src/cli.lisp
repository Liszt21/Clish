(in-package :clish)

(defun split (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
        as j = (position separator string :start i)
        as sub = (subseq string i j)
        unless (string= sub "") collect sub
        while j))

(defun join (str list)
  (if (null list)
      ""
    (let ((result (first list)))
      (dolist (item (cdr list))
        (setf result (concatenate 'string result str item)))
      result)))

(defun get-function-arguments (fn)
  (let ((stream (make-string-output-stream)))
    (describe fn stream)
    (car (member "Lambda-list:" (split (get-output-stream-string stream) #\NewLine) :test #'search))))

(defun decode-argument (source)
  (if (position #\- source)
      (intern (string-upcase (string-left-trim '(#\- ) source)) :keyword)
      source))

(defun decode-arguments (source)
  ;; (declare (source)) ;; TODO declare type
  (mapcar #'decode-argument (if (stringp source) (split source) source)))

(defun restruct-arguments (&optional arguments)
  (let ((keys '())
        (cmds '())
        (args (decode-arguments arguments)))
    (dolist (arg args)
      (if (keywordp arg)
          (progn
            (if (keywordp (car keys))
                (push t keys))
            (push arg keys))
          (if (keywordp (car keys))
              (push arg keys)
              (push arg cmds))))
    (when (keywordp (car keys))
      (push t keys))
    (append (reverse cmds) (reverse keys))))

(defclass command-line-interface ()
  ((commands :initform '())
   (name :initform nil)
   (doc :initform nil)
   (pre :initform (lambda (&optional command args) (format t "Execute command ~a with arguments ~a...~%" command args)))
   (post :initform (lambda (&optional command args result) (format t "Execute command ~a done ~%" command)))
   (help :initform (lambda (&optional command args) "Help"))))

(defmethod execute-command ((x command-line-interface) arguments)
  (let* ((cmds (slot-value x 'commands))
         (args (apply #'restruct-arguments arguments))
         (cmd (intern (string-upcase (car args))))
         (rest (cdr args))
         (fn (cdr (assoc cmd cmds))))
    (if (member :help args)
        (format t "Help command: ~a~%~A~%"
                cmd
                (if fn (get-function-arguments fn) "Command not registered"))
        (if fn
            (let* ((pre (apply (slot-value x 'pre) (list cmd rest)))
                   (rst (apply fn rest))
                   (post (apply (slot-value x 'post) (list cmd rest rst))))
                  rst)
            (format t "Command ~A not registered~%~A~%" cmd (helper x))))))

(defmethod dispatch ((x command-line-interface) arguments))

(defmethod register-command ((x command-line-interface) key value)
  (if (keywordp key)
      (setf (slot-value x (intern (string key) 'clish)) value)
      (push (cons key value) (slot-value x 'commands))))

(defmethod helper ((x command-line-interface))
  (let ((cmds (slot-value x 'commands)))
    (concatenate 'string (format nil "Commands:~%")
                 (join "" (mapcar (lambda (item) (format nil "    ~A: ~A~%" (car item) (get-function-arguments (cdr item)))) cmds)))))

(defmacro defcli (name &rest rest)
  `(progn
     (defparameter ,name (make-instance 'command-line-interface))
     (setf (slot-value ,name 'name) ',name)
     (mapcar (lambda (item) (register-command ,name (car item) (eval (cadr item)))) '(,@rest))
     (defun ,name (&rest args)
            ,(cadar (member :doc `(,@rest) :test #'equal :key #'car))
            (execute-command ,name args))))

(defun hello (name)
  (format nil "Hello ~a!!!" name))
