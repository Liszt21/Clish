(defpackage clish/tests
  (:use :cl :fiveam :clish))
(in-package :clish/tests)

(def-suite :clish)
(in-suite :clish)

(defcli cli-test (:docs "test-cli")
  (nil (lambda () "Default"))
  (hello (lambda (name) (format nil "Hello ~a!" name)))
  (keys (lambda (&key a b) (cons a b)))
  (argument (lambda (&rest args) args))
  (concat (lambda (a b) (concatenate 'string a b))))

(test test-cli-arguments
      (is (equal (cli-test "argument") '()))
      (is (equal (cli-test "argument" "a" "-b" "-c" "123" "--d" "#n123" "e-f") '("a" "e-f" :b t :c "123" :d 123)))
      (is (equal (cli-test "argument" "-a" "#x123" "-b" "#b111" "-c" "-d" "#c(1 2)") '(:a 291 :b 7 :c t :d #c(1 2)))))

(test test-cli
      (is (equal (cli-test) "Default"))
      (is (equal (cli-test "concat" "a" "b") "ab"))
      (is (equal (cli-test "hello" "clish") "Hello clish!"))
      (is (equal (cli-test "keys" "-a" "#x123" "-b" "#b111") (cons 291 7))))
