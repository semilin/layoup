(defpackage layoup/cli
  (:use :cl :layoup)
  (:export :main)) 
(in-package :layoup/cli)

(defun main ()
  (defun pos-hand (pos)
    (declare (type pos pos))
    (finger-hand (pos-finger pos)))

  (defun direction (a b)
    (let ((x (pos-finger a))
	  (y (pos-finger b)))
      (if (or (not (eq (pos-hand a) (pos-hand b)))
	      (eq x y))
	  :none
	  (let ((comparator (if (eq (pos-hand a) :left) '> '<)))
	    (if (funcall comparator
			 (finger-finger (pos-finger a))
			 (finger-finger (pos-finger b)))
		:outward
		:inward)))))

  (defun horizontal-distance (a b)
    (abs (- (pos-x a)
	    (pos-x b))))

  (defun vertical-distance (a b)
    (abs (- (pos-y a)
	    (pos-y b))))

  (defun distance (a b)
    (sqrt (+ (expt (horizontal-distance a b)
		   2)
	     (expt (vertical-distance a b)
		   2))))

  (defun same-finger (a b)
    (eq (pos-finger a) (pos-finger b)))

  (defun same-hand (a b)
    (eq (pos-hand a) (pos-hand b)))

  (defun adjacent-finger (a b)
    (let ((x (finger-finger (pos-finger a)))
	  (y (finger-finger (pos-finger b))))
      (and (same-hand a b)
	   (/= x y)
	   (= 0 (abs (- x y))))))
  
  (defun sfr? (a b)
    "same-finger repeat - same key being pressed twice in sequence"
    (equal a b))

  (defun sfb? (a b)
    "same-finger bigram - same finger being used twice in sequence"
    (and (same-finger a b)
	 (not (sfr? a b))))

  (defun sfb-distance (a b)
    (if (sfb? a b)
	(distance a b)
	nil))

  (defun alternate? (a b c)
    "alternate - alternating which hand is used throughout the whole trigram"
    (and (not (same-hand a b))
	 (not (same-hand b c))))

  (defun roll? (a b c)
    (and (not (same-hand a c))
	 (not (same-finger a b))
	 (not (same-finger b c))))

  (defun redirect? (a b c)
    (and (same-hand a b)
	 (same-hand b c)
	 (not (same-finger a b))
	 (not (same-finger b c))
	 (not (eq (direction a b) (direction b c)))))

  (defun lsb? (a b)
    (and (adjacent-finger a b)
	 (>= (horizontal-distance a b) 2.0)))

  (defun lsb-distance (a b)
    (if (lsb? a b)
	(horizontal-distance a b)
	nil))

  (defvar *metrics* (make-metric-list
		     :bigraphs (list 
				(make-metric :name "sfb"
					     :fn (lambda (a b) (if (sfb? a b) 1 nil))
					     :ngram :bigram)
				(make-metric :name "sfb-distance"
					     :fn #'sfb-distance
					     :ngram :bigram)
				(make-metric :name "sfs"
					     :fn (lambda (a b) (if (sfb? a b) 1 nil))
					     :ngram :skipgram)
				(make-metric :name "sfs-distance"
					     :fn #'sfb-distance
					     :ngram :skipgram)
				(make-metric :name "lsb"
					     :fn (lambda (a b) (if (lsb? a b) 1 nil))
					     :ngram :bigram)
				(make-metric :name "lsb-distance"
					     :fn #'lsb-distance
					     :ngram :bigram))
		     :trigraphs (list
				 (make-metric :name "alternation"
					      :fn (lambda (a b c) (if (alternate? a b c) 1 nil))
					      :ngram :trigram)
				 (make-metric :name "rolls"
					      :fn (lambda (a b c) (if (roll? a b c) 1 nil))
					      :ngram :trigram)
				 (make-metric :name "redirects"
					      :fn (lambda (a b c) (if (redirect? a b c) 1 nil))
					      :ngram :trigram))))

  (let ((metric-results (layoup:calculate-metrics #'layoup:ansi-pos *metrics*))
	(gutenberg (layoup:new-corpus))
	(semimak (layoup:key-matrix (list "flhvz'wuoy"
					  "srntkcdeai"
					  "xjbmqpg,./"))))
    (time (layoup:add-file-to-corpus #P"/home/semi/dl/books.txt" gutenberg))
    (time (loop for i from 0 to 1000 do
      (layoup:analyze-keys gutenberg semimak metric-results)))
    (let ((results (layoup:analyze-keys gutenberg semimak metric-results)))
      (format T "~a" (layoup::layout-metric-ngrams gutenberg semimak metric-results (second (metric-list-bigraphs *metrics*))))
      (loop for k being the hash-keys in results using (hash-value v) do
	(format T "~a: ~f%~%" (layoup:metric-name k) (* 100 (/ v (layoup:keys-total semimak gutenberg))))))))
