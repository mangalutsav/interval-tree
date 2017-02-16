(ns interval-tree.interval
	(:gen-class))

;; ## new 
;; Create a new interval.  Takes two arguments, 
;; the low end of the interval and the high end of the interval
;; in that order and returns the vector [low high].  This function 
;; can be used anywhere an interval needs to be constructed, or 
;; the vector [low high] can be used instead.
(defn new [low high] [low high])
 
;; ## low
;; Get the numerical value of the low end of the interval
(defn low [self] (self 0))

;; ## high
;; Get the numerical value of the high end of the interval
(defn high [self] (self 1))

;; ## overlaps? 
;; Takes two intervals and returns true if they overlap, false otherwise
(defn overlaps? [this that]
	(if (or (<= (low that) (high this) (high that))
	        (<= (low that) (low this) (high that))) 
		true 
		false))

;; ## includes?
;; Takes two intervals and returns true if the first interval
;; contains the second, false otherwise
(defn includes? [this that]
	(if (<= (low this) (low that) (high that) (high this))
		true 
		false))

;; ## intersects?
(defn intersects? [this that]
	(if (or (overlaps? this that) 
	        (includes? this that) 
	        (includes? that this))
		true 
		false))
(defn left? [this that]
	(if (< (high this) (low that))
		true 
		false))
(defn right? [this that]
	(if (> (low this) (high that))
		true 
		false))
(defn to-string [interval]
	(str (low interval) "," (high interval)))
(defn contains [interval point]
	(if (and (>= point (low interval))
	         (<= point (high interval)))
		true 
		false))
