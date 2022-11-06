(make-constraint-profile
 :name "default"
 :constraints (list
	       (make-constraint :metric (get-metric "redirects")
				:goal :less
				:threshold 3.5
				:weight 2.0)
	       (make-constraint :metric (get-metric "sfs-distance")
				:goal :less
				:threshold 6.5)
	       (make-constraint :metric (get-metric "pinky-sfb-distance")
				:goal :less
				:threshold 0.04)
	       (make-constraint :metric (get-metric "pinky-sfs-distance")
				:goal :less
				:threshold 0.40)
	       (make-constraint :metric (get-metric "ring-sfb-distance")
				:goal :less
				:threshold 0.40)
	       (make-constraint :metric (get-metric "ring-sfs-distance")
				:goal :less
				:threshold 1.30)
	       (make-constraint :metric (get-metric "pinky-sfr")
				:goal :less
				:threshold 0.35)))
