(asdf:defsystem "layoup"
  :version "0.1.0"
  :author "semi"
  :license "GPLv3"
  :depends-on ("alexandria")
  :components ((:module "src"
		:components
		((:file "main")
		 (:file "pos")
		 (:file "corpus")
		 (:file "layout")
		 (:file "analysis")
		 (:file "metrics"))))
  :description ""
  :in-order-to ((asdf:test-op (test-op "layoup/tests"))))

(asdf:defsystem "layoup/tests"
  :author "semi"
  :license "GPLv3"
  :depends-on ("layoup"
	       "rove")
  :components ((:module "tests"
		:components
		((:file "main"))))
  :description "Test system for layoup"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))

