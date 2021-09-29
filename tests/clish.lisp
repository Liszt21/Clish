(defpackage clish/tests
  (:use :cl :fiveam :clish))

(in-package :clish/tests)

(def-suite :clish)

(in-suite :clish)

(test test-5am
      (is (equal 1 1))
      (is (equal 2 2))
      (is (not (equal 2 1))))

(defcli test-cli
  (nil (lambda () "Default"))
  (hello (lambda (name) (format nil "Hello ~a!" name)))
  (argument (lambda (&rest args) args))
  (concat (lambda (a b) (concatenate 'string a b))))

(test test-cli
      (is (equal (test-cli) "Default"))
      (is (equal (test-cli "hello" "clish") "Hello clish!"))
      (is (equal (test-cli "concat" "a" "b") "ab"))
      (is (equal (test-cli "argument") '()))
      (is (equal (test-cli "argument" "a" "b") '("a" "b")))
      (is (equal (test-cli "argument" "a" "-b" "-c" "d") (list "a" :b t :c "d")))
      (is (equal (test-cli "argument" "a" '(b c)) '("a" (b c)))))

(test test-shell
      (is (equal (shell "exit -1") 255))
      (is (equal (shell "exit 0") 0)))
