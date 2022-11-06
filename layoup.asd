#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

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
		 (:file "analysis"))))
  :description "Declarative keyboard layout analyzer"
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

(asdf:defsystem "layoup/cli"
  :version "0.1.0"
  :author "semi"
  :license "GPLv3"
  :depends-on ("layoup"
	       "alexandria"
	       "cl-store"
	       "bordeaux-threads"
	       "arrows"
	       "with-user-abort"
	       "str"
	       "cl-ansi-text")
  :build-operation "program-op"
  :build-pathname "layoup"
  :entry-point "layoup/cli:main"
  :components ((:module "cli-src"
		:components
		((:file "main")
		 (:file "constraints")
		 (:file "analyze")
		 (:file "generate")
		 (:file "layouts")
		 (:file "commands"))))
  :description "CLI for layoup")
