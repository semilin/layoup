(make-constraint-profile
 :name "default"
 :constraints (list
	       (make-constraint :metric (get-metric "redirects")
				:goal :less
				:threshold 3.5)
	       (make-constraint :metric (get-metric "sfs-distance")
				:goal :less
				:threshold 6.5)))
