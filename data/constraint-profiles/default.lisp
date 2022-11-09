(make-constraint-profile
 :name "default"
 :constraints (list
	       (make-constraint :metric (get-metric "redirects")
				:goal :less
				:threshold 3.5
				:leniency 0.5)
	       (make-constraint :metric (get-metric "sfs-distance")
				:goal :less
				:threshold 6.0
				:leniency 0.8)
	       (make-constraint :metric (get-metric "pinky-sfb-distance")
				:goal :less
				:threshold 0.05)
	       (make-constraint :metric (get-metric "pinky-sfs-distance")
				:goal :less
				:threshold 0.40
				:leniency 0.5)
	       (make-constraint :metric (get-metric "ring-sfb-distance")
				:goal :less
				:threshold 0.40)
	       (make-constraint :metric (get-metric "ring-sfs-distance")
				:goal :less
				:threshold 0.65
				:leniency 1)
	       (make-constraint :metric (get-metric "middle-sfb-distance")
				:goal :less
				:threshold 0.5)
	       (make-constraint :metric (get-metric "middle-sfs-distance")
				:goal :less
				:threshold 2)
	       (make-constraint :metric (get-metric "pinky-sfr")
				:goal :less
				:threshold 0.35)
	       (make-constraint :metric (get-metric "lsb")
				:goal :less
				:threshold 1.5)
	       (make-constraint :metric (get-metric "middle-ring-scissor")
				:goal :less
				:threshold 0.08)
	       (make-constraint :metric (get-metric "pinky-ring-scissor")
				:goal :less
				:threshold 0.15)))

