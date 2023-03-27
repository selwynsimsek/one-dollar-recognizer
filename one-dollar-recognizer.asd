(defsystem "one-dollar-recognizer"
  :version "0.1.0"
  :author "Selwyn Simsek"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Implementation of the $1 gesture recognizer in Common Lisp"
  :in-order-to ((test-op (test-op "one-dollar-recognizer/tests"))))

(defsystem "one-dollar-recognizer/tests"
  :author "Selwyn Simsek"
  :license ""
  :depends-on ("one-dollar-recognizer"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for one-dollar-recognizer"
  :perform (test-op (op c) (symbol-call :rove :run c)))
