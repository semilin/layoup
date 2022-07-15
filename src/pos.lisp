(in-package :layoup)

(defstruct finger
  hand
  finger)

(defvar LT (make-finger :hand 'left :finger 0))
(defvar LI (make-finger :hand 'left :finger 1))
(defvar LM (make-finger :hand 'left :finger 2))
(defvar LR (make-finger :hand 'left :finger 3))
(defvar LP (make-finger :hand 'left :finger 4))

(defvar RT (make-finger :hand 'right :finger 0))
(defvar RI (make-finger :hand 'right :finger 1))
(defvar RM (make-finger :hand 'right :finger 2))
(defvar RR (make-finger :hand 'right :finger 3))
(defvar RP (make-finger :hand 'right :finger 4))

(defvar finger-list (list LP LR LM LI LT RT RI RM RR RP))

(defstruct pos
  finger
  col
  row
  x
  y)

(defun standard-finger (col)
  "Returns the finger that would be used to hit the key with standard fingering."
  (declare (type integer col))
  (cond ((= col 4) LI)
	((= col 5) RI)
	((<= col 3) (nth (max col 0) finger-list))
	((>= col 6) (nth (min col 9) finger-list))))

(defun ansi-pos (col row)
  "Returns the position that would apply to an ansi keyboard with standard fingering."
  (declare (type integer col)
	   (type integer row))
  (make-pos :finger (standard-finger col)
	    :col col
	    :row row
	    :x (+ col (cond ((= row 0) -0.25)
			    ((= row 2) 0.5)
			    (T 0)))
	    :y row))

(defun matrix-pos (col row)
  "Returns the position that would apply to a matrix keyboard."
  (declare (type integer col)
	   (type integer row))
  (make-pos :finger (standard-finger col)
	    :col col
	    :row row
	    :x col
	    :y row))


