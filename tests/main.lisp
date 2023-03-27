(defpackage one-dollar-recognizer/tests/main
  (:use :cl
        :one-dollar-recognizer
        :rove))
(in-package :one-dollar-recognizer/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :one-dollar-recognizer)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
