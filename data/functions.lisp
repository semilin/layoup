(use-package :layoup)

;;; UTILITIES

(defun pinky (pos)
  (or (eq LP (pos-finger pos))
      (eq RP (pos-finger pos))))

(defun ring (pos)
  (or (eq LR (pos-finger pos))
      (eq RR (pos-finger pos))))

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

(defun pinky-sfb? (a b)
  (if (and (pinky a)
	   (same-finger a b))
      (distance a b)
      nil))

(defun ring-sfb? (a b)
  (and (ring a)
       (same-finger a b)))

(defun pinky-sfb-distance (a b)
  (if (pinky-sfb? a b)
      (distance a b)
      nil))

(defun ring-sfb-distance (a b)
  (if (ring-sfb? a b)
      (distance a b)
      nil))
