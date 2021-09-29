(in-package :clish)

(defun shell (&rest cmds)
  (let ((command (join ";" cmds)))
    (third
     (multiple-value-list
      (uiop:run-program
       (format nil "~A~A"
               #+os-windows "powershell $OLDPWD=pwd;"
               #-os-windows ""
               command)
       :output :interactive
       :ignore-error-status t
       :error-output :interactive)))))
