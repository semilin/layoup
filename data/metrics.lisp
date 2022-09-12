(use-package :layoup)

(defun sfr? (a b)
  "same-finger repeat - same key being pressed twice in sequence"
  (equal a b))

(defun sfb? (a b)
  "same-finger bigram - same finger being used twice in sequence"
  (and (eq (pos-finger a)
	   (pos-finger b))
       (not (sfr? a b))))

(defun alternate? (a b c)
  "alternate - alternating which hand is used throughout the whole trigram"
  (and (not (eq (finger-hand a)
		(finger-hand b)))
       (not (eq (finger-hand b)
		(finger-hand c)))))

(defvar *metrics* (make-metric-list
	     :bigraphs (list 
			(make-metric :name "sfb"
				     :fn (lambda (a b) (if (sfb? a b) 1 0))
				     :ngram :bigram)
			(make-metric :name "sfs"
				     :fn (lambda (a b) (if (sfb? a b) 1 0))
				     :ngram :skipgram))
	     :trigraphs (list
			 (make-metric :name "alternate"
				      :fn (lambda (a b c) (if (alternate? a b c) 1 0))
				      :ngram :trigram))))
