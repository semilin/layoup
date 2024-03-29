(in-package :layoup/cli)

(defvar *commands*
  (list
   (make-command :name "analyze"
		 :aliases '("a" "stats")
		 :description "Prints the metric statistics for a layout."
		 :arguments (list (make-argument :name "layout"
						 :type :layout))
		 :function #'analyze)
   (make-command :name "view"
		 :aliases '("v")
		 :description "Prints the matrix for a layout."
		 :arguments (list (make-argument :name "layout"
						 :type :layout))
		 :function (lambda (l) (print-matrix (layout-matrix l))))
   (make-command :name "most"
		 :aliases '("m")
		 :description "Lists the layouts with the most of a metric."
		 :arguments (list (make-argument :name "metric"
						 :type :metric))
		 :function #'most)
   (make-command :name "least"
		 :aliases '("l")
		 :description "Lists the layouts with the least of a metric."
		 :arguments (list (make-argument :name "metric"
						 :type :metric))
		 :function #'least)
   (make-command :name "ngrams"
		 :aliases '("n")
		 :description "Prints the most frequent ngrams for a metric on a layout."
		 :arguments (list (make-argument :name "layout"
						 :type :layout)
				  (make-argument :name "metric"
						 :type :metric))
		 :function #'list-ngrams)
   (make-command :name "random-layout"
		 :aliases '("rl")
		 :description "Generates a random layout."
		 :arguments ()
		 :function (lambda () (print-matrix (random-layout))))
   (make-command :name "constrained-anneal"
		 :aliases '("ca")
		 :description "Generates a layout with an optimized metric using a constraint profile."
		 :examples '("constrained-anneal less sfb default")
		 :arguments (list (make-argument :name "optimized-comparator"
						 :type :comparator)
				  (make-argument :name "optimized-metric"
						 :type :metric)
				  (make-argument :name "constraint-profile"
						 :type :profile))
		 :function #'constraint-anneal)   
   (make-command :name "constrainted-anneal-multithreaded"
		 :aliases '("cam")
		 :description "Performs multiple constrained anneals at the same time."
		 :arguments (list (make-argument :name "optimized-comparator"
						 :type :comparator)
				  (make-argument :name "optimized-metric"
						 :type :metric)
				  (make-argument :name "constraint-profile"
						 :type :profile))
		 :function #'constraint-multithreaded-anneal)
   (make-command :name "first-matching"
		 :aliases '("fm")
		 :description "Finds the first layout that matches the constraints."
		 :examples '("find-matching default")
		 :arguments (list (make-argument :name "constraint-profile"
						 :type :profile))
		 :function #'first-matching)
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
   (make-command :name "list-metrics"
		 :aliases '("lm")
		 :description "Lists the available metrics."
		 :arguments ()
		 :function #'list-metrics)
   (make-command :name "profile"
		 :aliases '("p")
		 :description "Lists the constraints from the profile."
		 :arguments (list (make-argument :name "profile"
						 :type :profile))
		 :function #'profile)
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
   (make-command :name "genkey-sync"
		 :aliases ()
		 :description "Imports your genkey layouts directory into layoup."
		 :arguments (list (make-argument :name "directory"
						 :type :path))
		 :function #'convert-genkey)
   (make-command :name "dof-sync"
		 :aliases ()
		 :description "Synchronizes your local layouts with DOF."
		 :arguments ()
		 :function #'dof-sync)
   (make-command :name "help"
		 :aliases '("hwat" "?")
		 :description "Prints a help message and list of commands."
		 :arguments nil
		 :function #'help)
   (make-command :name "quit"
		 :aliases '("exit" "q")
		 :description "Quits the program."
		 :arguments nil
		 :function #'graceful-quit)))
