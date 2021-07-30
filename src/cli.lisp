(in-package :clish)

(defun split (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
        as j = (position separator string :start i)
        as sub = (subseq string i j)
        unless (string= sub "") collect sub
        while j))

(defun decode-argument (source)
  (if (position #\- source)
      (intern (string-upcase (string-left-trim '(#\- ) source)) :keyword)
      source))

(defun decode-arguments (source)
  ;; (declare (source)) ;; TODO declare type
  (mapcar #'decode-argument (if (stringp source) (split source) source)))

(defun restruct-arguments (arguments)
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
  ((commands :initform '())))

(defmethod execute-command ((x command-line-interface) arguments)
  (let* ((cmds (slot-value x 'commands))
         (args (restruct-arguments arguments))
         (cmd (car args))
         (rest (cdr args))
         (fn (cdr (assoc cmd cmds :test #'equal))))
    (if fn
        (apply fn rest)
        (if (or (equal cmd "help") (member :help rest))
            (pprint "Help")
            (progn
              (pprint "command not exist")
              (pprint args)
              (pprint cmds))))))

(defmethod register-command ((x command-line-interface) name function)
  (push (cons name function) (slot-value x 'commands)))

(defmacro defcli (name &rest rest)
  (pprint rest)
  `(progn
     (defparameter ,name (make-instance 'command-line-interface))
     (mapcar (lambda (item) (register-command ,name (car item) (eval (cadr item)))) '(,@rest))
     (defun ,name (&rest args)
            (execute-command ,name args))))

;; (defun hello (name)
;;   (format nil "Hello ~a" name))

;; ;; (pprint (macroexpand '(defcli cli ("hello" #'hello) ("foo" (lambda () :bar)))))
;; (defcli cli ("hello" #'hello) ("foo" (lambda () :bar)))

;; (slot-value cli 'commands)

;; (cli "hello clish")
;; (cli "foo")
