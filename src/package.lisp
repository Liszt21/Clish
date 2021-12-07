(defpackage clish
  (:use :cl)
  (:import-from :str :join :split)
  (:export
   :main
   :repl
   :shell
   :defcli
   :cli
   #+os-windows :rosw
   :with-profile
   :generate-alias-define))
