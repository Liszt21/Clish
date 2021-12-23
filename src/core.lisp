(in-package :clish)

(defparameter *hist* 0)

(defun promot ()
  (format t "~A:[~D]>~%☰ " (uiop:getenv #+os-windows "USERNAME" #-os-windows "USER") *hist*)
  (setf *hist* (1+ *hist*)))

(defmacro handle-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition (err)
        (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

(defun repl ()
  (set-macro-character #\! #'shell-mode)
  (loop (handle-errors
         (promot)
         (setf +++ ++ ++ + + - - (read))
         (when (position - '((quit) (exit)) :test #'equal)
           (format t "Exit...")
           (return-from repl))
         (setf /// // // / / (multiple-value-list (eval -)))
         (setf *** ** ** * * (first /))
         (format t "~& --> ~{~S~^ ;~%     ~}~%" /))))

(defun read-shell-script (stream)
  (declare (ignorable stream))
  (loop for c = (read-char)
        while (not (char= c #\NewLine))
        collect c))

(defun shell-mode (stream char)
  (declare (ignorable stream char))
  (let ((script (read-line stream)));; FIXME need double #\NewLine
    (shell script)))

(defun main ()
  (repl))

(defcli cli
  (nil #'repl)
  ("help" (lambda (command) (format t "Help ~a" command))))

