(defsystem "clish"
  :version "0.1.0"
  :author "Liszt21"
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "core"))))
  :description ""
  :entry-point "clish:main"
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
