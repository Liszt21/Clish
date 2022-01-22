(in-package :clish)

(defpackage :command)

;; utilities
(defun get-function-arguments (fn)
  (let ((stream (make-string-output-stream)))
    (describe fn stream)
    (car (member "Lambda-list:" (str:split #\NewLine (get-output-stream-string stream)) :test #'search))))

(defun trim (string &optional (char #\Space))
 (if (char= (char string 0) char)
   (trim (subseq string 1) char)
   string))

(defmacro start-with-p (string prefix &rest prefixs)
  `(or (str:starts-with? ,prefix ,string) ,@(loop for pre in prefixs collect `(str:starts-with? ,pre ,string))))

(defun read-and-eval (stream char1 char2) (declare (ignore char1 char2)) (read stream t nil t))

(set-dispatch-macro-character #\# #\n #'read-and-eval)

(defun lisp-code-p (string) (start-with-p string "'(" "#(" "(" "#x" "#b" "#c" "#n"))

(defun parse-argument (argument)
  (cond
    ((not (stringp argument)) argument)
    ((lisp-code-p argument) (with-input-from-string (in argument) (eval (read in))))
    ((start-with-p argument "-") (intern (string-upcase (trim argument #\-)) 'keyword))
    (t argument)))

(defun wrap-arguments (&rest args)
  (let ((wrapped (mapcar #'parse-argument args))
        (cmds '())
        (keys '()))
    (dolist (arg wrapped)
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
    (append (if (zerop (length cmds)) '(nil)) (reverse cmds) (reverse keys))))

(defun execute-command (command arguments)
  (destructuring-bind (name fn &rest options) command
    (declare (ignore name options))
    (if (member :help arguments)
        (format t "~A~%" (get-function-arguments (ensure-function fn)))
        (apply (if (functionp fn) fn (eval fn)) arguments))))

(defclass command-line-interface ()
  ((cmds :accessor cli-cmds :initarg :cmds :initform '())
   (docs :accessor cli-docs :initarg :docs :initform nil)
   (help :initarg :help :initform nil)
   (after :initarg :after :initform nil)
   (before :initarg :before :initform nil)))

(defmethod display-commands ((x command-line-interface))
  (let ((cmds (cli-cmds x)))
    (format t "~{  ~A~}~%"
            (mapcar (lambda (item)
                      (format nil "~9:A: ~A~%" (car item) (get-function-arguments (cdr item))))
                    cmds))))

(defmethod dispatch-command ((instance command-line-interface) arguments)
  (let* ((commands (cli-cmds instance))
         (arguments (apply #'wrap-arguments arguments))
         (cmd (assoc (string-upcase (car arguments)) commands :test #'string=))
         (args (cdr arguments)))
    (if cmd
        (let ((before (slot-value instance 'before))
              (after (slot-value instance 'after))
              (result (execute-command cmd args)))
          (when before (funcall (ensure-function before) args))
          (when after (funcall (ensure-function after) args result))
          result)
        (display-commands instance))))

(defmacro defcli (name (&rest config &key docs &allow-other-keys) &rest cmds)
  `(progn
     (defparameter ,name (make-instance 'command-line-interface ,@config :cmds ',cmds))
     (defun ,name (&rest arguments)
       ,docs
       (dispatch-command ,name arguments))))

#+os-windows
(progn
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

  (defcli rosw ()
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

