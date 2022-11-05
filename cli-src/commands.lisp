(in-package :layoup/cli)

(defvar *commands*
  (list
   (make-command :name "analyze"
		 :aliases '("a" "stats")
		 :description "Prints the metric statistics for a layout."
		 :arguments (list (make-argument :name "layout"
						 :type :layout))
		 :function #'analyze)
   (make-command :name "add-corpus"
		 :aliases '("ac")
		 :description "Loads a corpus from a text file and adds it to the list of corpora."
		 :arguments (list (make-argument :name "path"
						 :type :path)
				  (make-argument :name "corpus-name"
						 :type :string))
		 :function #'add-corpus)
   (make-command :name "list-corpora"
		 :aliases '("lc")
		 :description "Lists the available corpora."
		 :arguments nil
		 :function #'list-corpora)
   (make-command :name "remove-corpus"
		 :aliases '("rc")
		 :description "Deletes the given corpus."
		 :arguments (list (make-argument :name "corpus"
						 :type :corpus))
		 :function #'remove-corpus)
   (make-command :name "default-keyboard"
		 :aliases '("dk")
		 :description "Sets the default keyboard."
		 :arguments (list (make-argument :name "keyboard"
						 :type :keyboard))
		 :function #'default-keyboard)
   (make-command :name "default-corpus"
		 :aliases '("dc")
		 :description "Sets the default corpus."
		 :arguments (list (make-argument :name "corpus"
						 :type :corpus))
		 :function #'default-corpus)
   (make-command :name "import-genkey"
		 :aliases ()
		 :description "Imports your genkey layouts directory into layoup."
		 :arguments (list (make-argument :name "directory"
						 :type :path))
		 :function #'convert-genkey)
   (make-command :name "help"
		 :aliases '("hwat" "?")
		 :description "Prints a help message and list of commands."
		 :arguments nil
		 :function #'help)
   (make-command :name "quit"
		 :aliases '("exit" "q")
		 :description "Quits the program."
		 :arguments nil
		 :function (lambda () (prepare-quit) (uiop:quit)))))
