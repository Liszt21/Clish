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
   (default :initform (lambda (&rest args) (format t "Command ~A with args: [~{ ~A~} ] not registered~%" (car args) (cdr args))))
   (help :initform (lambda (&optional command args) "Help"))))

(defmethod register-command ((x command-line-interface) key value)
  (if (keywordp key)
      (setf (slot-value x (intern (string key) 'clish)) value)
    (push (cons (intern (string key) 'command) value) (slot-value x 'commands))))

(defmethod helper ((x command-line-interface))
  (format t "Helper for ~A:~%" (slot-value x 'name))
  (let ((cmds (slot-value x 'commands)))
    (concatenate 'string (format nil "Commands:~%")
                 (join "" (mapcar (lambda (item) (format nil "    ~A: ~A~%" (car item) (get-function-arguments (cdr item)))) cmds)))))

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
          (if (car args)
            (apply (slot-value x 'default) args)
            (helper x))))))

(defmacro defcli (name &rest rest)
  `(progn
     (defparameter ,name (make-instance 'command-line-interface))
     (setf (slot-value ,name 'name) ',name)
     (mapcar (lambda (item) (register-command ,name (car item) (eval (cadr item)))) '(,@rest))
     (defun ,name (&rest args)
            ,(cadar (member :doc `(,@rest) :test #'equal :key #'car))
            (execute-command ,name args))))

#+os-windows
(progn
  (defun flatten (x)
    (labels ((rec (x acc)
               (cond
                 ((null x) acc)
                 ((atom x) (cons x acc))
                 (t (rec (car x) (rec (cdr x) acc))))))
      (rec x nil)))

  (defun detect-roswell ()
    (loop for folder in (append
                          '("~/.roswell")
                          (list #+os-windows
                                (when (uiop:getenv "SCOOP")
                                  (concatenate 'string
                                               (uiop:getenv "SCOOP")
                                               "/apps/roswell/current/"))
                                (uiop:getenv "ROSWELL")))
          when (and folder (probe-file folder))
          collect (probe-file folder)))

  (defun detect-roswell-scripts ()
    (flatten
      (loop for folder in (detect-roswell)
            for bin = (merge-pathnames
                        (make-pathname :name :wild)
                        (merge-pathnames "bin/" folder))
            for ql-bin = (merge-pathnames
                          (make-pathname :name :wild :type "ros")
                          (merge-pathnames "lisp/quicklisp/bin/" folder))
            collect (directory bin)
            collect (directory ql-bin))))

  (defun generate-alias-function (file)
    (format nil "function ~A {~%  ros ~A $args~%}~%" (pathname-name file) file))

  (defcli rosw
      (nil (lambda ()
             (format t "Write alias to clish#rosw~%")
             (with-profile (ctx :section "rosw")
               (setf ctx '())
               (dolist (file (reverse (detect-roswell-scripts)))
                 (push (generate-alias-function file) ctx))
               (pprint ctx)
               (format t "~%"))))
    (clean (lambda ()
             (format t "Clean alias in clish#rosw~%")
             (with-profile (ctx :section "rosw")
               (setf ctx nil))))))
