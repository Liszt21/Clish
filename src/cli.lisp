(in-package :clish)

(defpackage :command)

(defun flatten (x)
  (labels ((rec (x acc)
            (cond
              ((null x) acc)
              ((atom x) (cons x acc))
              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun get-function-arguments (fn)
  (let ((stream (make-string-output-stream)))
    (describe fn stream)
    (car (member "Lambda-list:" (split #\NewLine (get-output-stream-string stream)) :test #'search))))

(defun decode-argument (source)
  (if (position #\- source)
      (intern (string-upcase (string-left-trim '(#\- ) source)) :keyword)
      source))

(defun decode-arguments (source)
  ;; (declare (source)) ;; TODO declare type
  (mapcar #'decode-argument source))

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

(defun parse-arguments (&rest args)
  (flatten
   (loop for arg in args
         collect (cond
                   ((stringp arg) (restruct-arguments (split " " arg)))
                   ((consp arg) (apply #'parse-arguments arg))
                   (t nil)))))

(defclass command-line-interface ()
  ((commands :initform '())
   (name :initform nil)
   (doc :initform nil)
   (pre :initform (lambda (&optional command args) (format t "Execute command ~a with arguments ~a...~%" command args)))
   (post :initform (lambda (&optional command args result) (format t "Execute command ~a done ~%" command)))
   (help :initform (lambda (&optional command args) "Help"))))

(defmethod execute-command ((x command-line-interface) arguments)
  (let* ((cmds (slot-value x 'commands))
         (args (parse-arguments arguments))
         (cmd (intern (string-upcase (string (car arguments))) 'command))
         (rest (cdr args))
         (fn (cdr (assoc cmd cmds))))
    (if (member :help args)
        (format t "~%Help command: ~a~%~A~%"
                cmd
                (if fn (get-function-arguments fn) "Command not registered"))
        (if fn
            (let* ((pre (apply (slot-value x 'pre) (list cmd rest)))
                   (rst (apply fn rest))
                   (post (apply (slot-value x 'post) (list cmd rest rst))))
                  rst)
            (format t "~%Command ~A not registered~%~A~%" cmd (helper x))))))

(defmethod register-command ((x command-line-interface) key value)
  (if (keywordp key)
      (setf (slot-value x (intern (string key) 'clish)) value)
      (push (cons (intern (string key) 'command) value) (slot-value x 'commands))))

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
