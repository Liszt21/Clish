(defsystem "clish"
  :version "0.1.2"
  :author "Liszt21"
  :license ""
  :depends-on ("str" "likit" "alexandria" "inferior-shell")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "cli")
                 (:file "core"))))
  :description ""
  :in-order-to ((test-op (test-op "clish/tests"))))

(defsystem "clish/tests"
  :author "Liszt21"
  :license ""
  :depends-on ("clish"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "clish"))))
  :description "Test system for Clish"
  :perform (test-op (op c) (symbol-call :fiveam :run! :clish)))
