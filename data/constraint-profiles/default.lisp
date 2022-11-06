(make-constraint-profile
 :name "default"
 :constraints (list
	       (make-constraint :metric (get-metric "redirects")
				:goal :less
				:threshold 3.4
				:weight 1.0)
	       (make-constraint :metric (get-metric "sfs-distance")
				:goal :less
				:threshold 6.0)
	       (make-constraint :metric (get-metric "pinky-sfb-distance")
				:goal :less
				:threshold 0.04
				:weight 6)
	       (make-constraint :metric (get-metric "pinky-sfs-distance")
				:goal :less
				:threshold 0.40
				:weight 6)
	       (make-constraint :metric (get-metric "ring-sfb-distance")
				:goal :less
				:threshold 0.40
				:weight 4)
	       (make-constraint :metric (get-metric "ring-sfs-distance")
				:goal :less
				:threshold 0.80)
	       (make-constraint :metric (get-metric "pinky-sfr")
				:goal :less
				:threshold 0.35)
	       (make-constraint :metric (get-metric "lsb")
				:goal :less
				:threshold 1.75)))
