(defpackage clish/tests
  (:use :cl :fiveam :clish))

(in-package :clish/tests)

(def-suite :clish)

(in-suite :clish)

(test test-5am
      (is (equal 1 1))
      (is (equal 2 2))
      (is (not (equal 2 1))))

(test test-cli
      (is (equal (restruct-arguments "a b -c d") '("a" "b" :c "d")))
      (is (equal (restruct-arguments "a -b c d") '("a" "d" :b "c")))
      (is (equal (restruct-arguments '("a" "-b" "c" "d")) '("a" "d" :b "c")))
      (is (equal (progn (defcli cli ("hello" (lambda (name) (format nil "Hello ~a!" name)))) (cli "hello clish"))
                 "Hello clish!")))
