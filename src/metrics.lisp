(in-package :layoup)

(defun sfr? (a b) (equal a b))

(defun sfb? (a b) (and (eq (pos-finger a)
			   (pos-finger b))
		       (not (sfr? a b))))

(defvar *metrics* (make-metric-list
		   :bigraphs (list (make-metric :name "sfr"
						:fn (lambda (a b) (if (sfr? a b) 1 0)))
				   (make-metric :name "sfb"
						:fn (lambda (a b) (if (sfb? a b) 1 0))))
		   :trigraphs nil))
