(in-package :common-lisp-user)
(require :asdf)

(asdf:initialize-output-translations
 `(:output-translations
   (t ,(merge-pathnames (make-pathname
                         :directory '(:relative "cache" :wild-inferiors)
                         :name :wild :type :wild)
                        *load-truename*))
   :disable-cache
   :ignore-inherited-configuration))

(asdf:load-asd (probe-file "./clish.asd"))

(if (not (asdf:find-system "clish"))
    (progn
     (format t "~&Unable to find the clish ASDF system definition.~%")
     (uiop:quit 1))
    (format t "~&Found clish ASDF system definition.~%"))


(defun main (&rest argv)
    (let ((command (car argv)))
        (when (equal "test" command)
            (asdf:test-system "clish")
            (uiop:quit 0))))
