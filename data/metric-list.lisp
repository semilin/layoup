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
			 :ngram :bigram)
	    (make-metric :name "pinky-sfb-distance"
			 :fn #'pinky-sfb-distance
			 :ngram :bigram)
	    (make-metric :name "pinky-sfs-distance"
			 :fn #'pinky-sfb-distance
			 :ngram :skipgram)
	    (make-metric :name "ring-sfb-distance"
			 :fn #'ring-sfb-distance
			 :ngram :bigram)
	    (make-metric :name "ring-sfs-distance"
			 :fn #'ring-sfb-distance
			 :ngram :skipgram)
	    (make-metric :name "middle-sfb-distance"
			 :fn #'middle-sfb-distance
			 :ngram :bigram)
	    (make-metric :name "middle-sfs-distance"
			 :fn #'middle-sfb-distance
			 :ngram :skipgram)
	    (make-metric :name "pinky-sfr"
			 :fn (lambda (a b) (if (pinky-sfr? a b) 1.0 nil))
			 :ngram :bigram)
	    (make-metric :name "bottom-row"
			 :fn #'bottom-row-usage
			 :ngram :bigram)
	    (make-metric :name "middle-ring-scissor"
			 :fn (lambda (a b) (if (middle-ring-scissor? a b) 1.0 nil))
			 :ngram :bigram)
	    (make-metric :name "pinky-ring-scissor"
			 :fn (lambda (a b) (if (pinky-ring-scissor? a b) 1.0 nil))
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
