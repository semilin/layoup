(use-package :layoup)

;;; UTILITIES

(defun pinky? (pos)
  (or (eq LP (pos-finger pos))
      (eq RP (pos-finger pos))))

(defun ring? (pos)
  (or (eq LR (pos-finger pos))
      (eq RR (pos-finger pos))))

(defun middle? (pos)
  (or (eq LM (pos-finger pos))
      (eq RM (pos-finger pos))))

(defun index? (pos)
  (or (eq LI (pos-finger pos))
      (eq RI (pos-finger pos))))


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
  (coerce (abs (- (pos-x a)
		  (pos-x b)))
	  'float))

(defun vertical-distance (a b)
  (coerce (abs (- (pos-y a)
		  (pos-y b)))
	  'float))

(defun distance (a b)
  (the float (sqrt (+ (expt (horizontal-distance a b)
			    2)
		      (expt (vertical-distance a b)
			    2)))))

(defun same-finger (a b)
  (eq (pos-finger a) (pos-finger b)))

(defun same-hand (a b)
  (eq (pos-hand a) (pos-hand b)))

(defun adjacent-finger (a b)
  (let ((x (finger-finger (pos-finger a)))
	(y (finger-finger (pos-finger b))))
    (and (same-hand a b)
	 (/= x y)
	 (= 1 (abs (- x y))))))

;;; METRICS

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

(defmacro finger-sfb (finger-fn)
  `(and (,finger-fn a)
	(same-finger a b)))

(defmacro finger-distance (sfb-fn)
  `(if (,sfb-fn a b)
       (distance a b)
       nil))

(defun pinky-sfb? (a b)
  (finger-sfb pinky?))

(defun ring-sfb? (a b)
  (finger-sfb ring?))

(defun middle-sfb? (a b)
  (finger-sfb middle?))

(defun index-sfb? (a b)
  (finger-sfb index?))

(defun pinky-sfb-distance (a b)
  (finger-distance pinky-sfb?))

(defun ring-sfb-distance (a b)
  (finger-distance ring-sfb?))

(defun middle-sfb-distance (a b)
  (finger-distance middle-sfb?))

(defun index-sfb-distance (a b)
  (finger-distance index-sfb?))

(defun pinky-sfr? (a b)
  (and (sfr? a b)
       (pinky? a)))

(defun bottom-row-usage (a b)
  (if (and (= (pos-row a) 2)
	   (= (pos-row b) 2))
      1.0
      nil))

(defun middle-ring-scissor? (a b)
  (and (adjacent-finger a b)
       (or (and (middle? a)
		(ring? b))
	   (and (ring? a)
		(middle? b)))       
       (>= (vertical-distance a b) 2)))

(defun pinky-ring-scissor? (a b)
  (and (adjacent-finger a b)
       (or (and (pinky? a)
		(ring? b))
	   (and (ring? a)
		(pinky? b)))
       (>= (vertical-distance a b) 2)))
