(in-package :layoup/cli)

(defstruct constraint
  (metric nil :type metric)
  (goal nil :type symbol)
  (threshold nil :type number)
  (weight 1.0 :type number))

(defstruct constraint-profile
  (name nil :type string)
  (constraints nil :type list))
