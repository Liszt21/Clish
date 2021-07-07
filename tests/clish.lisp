(defpackage clish/tests
  (:use :cl :fiveam :clish))

(in-package :clish/tests)

(def-suite :clish)

(in-suite :clish)

(test (test-5am :compile-at :run-time)
  (is (equal 1 1))
  (is (not (equal 2 1))))
