(defpackage clish
  (:use :cl :alexandria :inferior-shell :likit)
  (:import-from :str :join :split)
  (:export
   :main
   :repl
   :defcli
   :cli
   #+os-windows :rosw
   :with-profile
   :generate-alias-define))
