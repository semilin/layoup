(make-metric-list
 :bigraphs (list 
	    (make-metric :name "sfb"
			 :fn (lambda (a b) (if (sfb? a b) 1.0 nil))
			 :ngram :bigram)
	    (make-metric :name "sfb-distance"
			 :fn #'sfb-distance
			 :ngram :bigram)
	    (make-metric :name "sfs"
			 :fn (lambda (a b) (if (sfb? a b) 1.0 nil))
			 :ngram :skipgram)
	    (make-metric :name "sfs-distance"
			 :fn #'sfb-distance
			 :ngram :skipgram)
	    (make-metric :name "lsb"
			 :fn (lambda (a b) (if (lsb? a b) 1.0 nil))
			 :ngram :bigram)
	    (make-metric :name "lsb-distance"
			 :fn #'lsb-distance
			 :ngram :bigram))
 :trigraphs (list
	     (make-metric :name "alternation"
			  :fn (lambda (a b c) (if (alternate? a b c) 1.0 nil))
			  :ngram :trigram)
	     (make-metric :name "rolls"
			  :fn (lambda (a b c) (if (roll? a b c) 1.0 nil))
			  :ngram :trigram)
	     (make-metric :name "redirects"
			  :fn (lambda (a b c) (if (redirect? a b c) 1.0 nil))
			  :ngram :trigram)))
