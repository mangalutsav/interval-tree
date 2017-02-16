(ns interval-tree.core
	(:gen-class)
	(:require [interval]))
(def red "red")
(def black "black")
(defn low [interval] (interval/low interval))
(defn high [interval] (interval/high interval))
(defn node 
	([]
		(ref {:key nil 
		      :value nil
		      :color nil 
		    s  :left nil
		      :right nil
		      :parent nil
		      :interval nil
		      :max nil}))
	([key value color left right parent interval max]
		(let [self (node)]
			(dosync
				(alter self assoc :key key)
				(alter self assoc :value value)
				(alter self assoc :color color)
				(alter self assoc :left left)
				(alter self assoc :right right)
				(alter self assoc :parent parent)
				(alter self assoc :interval interval)
				(alter self assoc :max max))
			self)))

(defn get-key [self] (:key @self))
(defn get-value [self] (:value @self))
(defn get-color [self] (:color @self))
(defn get-left [self] (:left @self))
(defn get-right [self] (:right @self))
(defn get-parent [self] (:parent @self))
(defn get-interval [self] (:interval @self))
(defn get-max [self] (:max @self))
(defn set-key [self key] (dosync (alter self assoc :key key)))
(defn set-value [self value] (dosync (alter self assoc :value value)))
(defn set-color [self color] (dosync (alter self assoc :color color)))
(defn set-left [self left] (dosync (alter self assoc :left left)))
(defn set-right [self right] (dosync (alter self assoc :right right)))
(defn set-parent [self parent] (dosync (alter self assoc :parent parent)))
(defn set-interval [self interval] (dosync (alter self assoc :interval interval)))
(defn set-max [self max] (dosync (alter self assoc :max max)))
(defn new []
	(let [sentinel (node nil nil black nil nil nil nil nil)
	      root (node nil nil black sentinel sentinel sentinel nil nil)
	      self (ref {:sentinel sentinel :root root})]
		self))
(defn get-sentinel [self] (:sentinel @self))
(defn get-root [self] (:root @self))
(defn set-root [self node] (dosync (alter self assoc :root node)))
(defn pretty-print 
	([tree]
		(if (not= (get-key (get-root tree)) nil) 
			(pretty-print (get-root tree) 0 (get-sentinel tree))))
	([node indent sentinel]
		(let [indent-string (apply str (repeat indent " ")) interval (get-interval node)]
			(if (= node sentinel)
				(println (str indent-string "sentinel"))
				(do
					(if (= (get-key (get-parent node)) nil)
						(println 
							(str indent-string (get-key node) " : " (get-color node) 
						     	" [interval " (low interval) " " (high interval) "]"
								" [max " (get-max node) "]"))
						(println 
							(str indent-string (get-key node) " : " (get-color node) 
						     	" [parent-key " (get-key (get-parent node)) "]" 
						     	" [interval " (low interval) " " (high interval) "]"
								" [max " (get-max node) "]")))
					(pretty-print (get-left node) (+ indent 2) sentinel)
					(pretty-print (get-right node) (+ indent 2) sentinel))))))
(defn max-of-three [element-one element-two element-three]
	(apply max
		(remove nil? 
			[element-one element-two element-three])))
(defn recursive-max [tree node]
	(if (= node (get-sentinel tree))
		nil
		(max-of-three
			(high (get-interval node))
			(recursive-max tree (get-left node))
			(recursive-max tree (get-right node))))) 
(defn check-max-interval [tree node]
	(if (= node (get-sentinel tree))
		true
		(let [condition-one 
			(= 
				(get-max node) 
				(max-of-three 
					(high (get-interval node))
					(recursive-max tree (get-left node))
					(recursive-max tree (get-right node))))
		      condition-two (check-max-interval tree (get-left node))
		      condition-three (check-max-interval tree (get-right node))]
			(if (= condition-one condition-two condition-three true)
				true
				false))))  	
(defn check-parent-child-links [tree node]
	(if (= node (get-sentinel tree))
		true
		(let [left (get-left node) 
		      right (get-right node)
			  sentinel (get-sentinel tree)
		      condition-one (if (or (= left sentinel) (= node (get-parent left))) true false)
		      condition-two (if (or (= right sentinel) (= node (get-parent right))) true false)
		      condition-three (check-parent-child-links tree left)
		      condition-four (check-parent-child-links tree right)] 
			(if (and condition-one condition-two condition-three condition-four)
				true
				false)))) 
(defn check-double-reds [tree node]
	(if (= node (get-sentinel tree))
		true
		(let [left (get-left node) right (get-right node) color (get-color node)]
			(if (= color (get-color left) red)
				false
				(if (= color (get-color right) red)
					false
					(if (and (= (check-double-reds tree left) true)
					         (= (check-double-reds tree right) true))
						true
						false))))))
(defn health-check [tree]
	(let [root (get-root tree)]
		(if (and (check-max-interval tree root)
		         (check-parent-child-links tree root)
		         (check-double-reds tree root))
		true
		false)))
(defn add-to-result [node results]
	(dosync 
		(alter results conj [(get-interval node) (get-value node)])))
(defn vectorize 
	([tree] 
		(let [results (ref [])]
			(vectorize tree (get-root tree) (get-sentinel tree) results)
			@results))
	([tree node sentinel results]
		(if (not= node sentinel)
			(do 
				(add-to-result node results)
					(vectorize tree (get-left node) sentinel results)
					(vectorize tree (get-right node) sentinel results)))))
(defn update-max [node]
	(set-max node
		(max-of-three 
			(get-max (get-left node)) 
			(get-max (get-right node)) 
			(high (get-interval node)))))	
(defn update-max-up-tree [tree node]
	(dosync
		(let [w (ref node) sentinel (get-sentinel tree)]
			(loop []
				(if (and (not= @w sentinel) (not= @w nil))
					(do
						(update-max @w)
						(ref-set w (get-parent @w))
						(recur)))))))
(defn left-rotate [tree node]
	(let [y (get-right node)]
		(set-right node (get-left y))
		(if (not= (get-left y) (get-sentinel tree))
			(set-parent (get-left y) node))
		(set-parent y (get-parent node))
		(if (= (get-parent node) (get-sentinel tree))
			(set-root tree y)
			(if (= node (get-left (get-parent node)))
				(set-left (get-parent node) y)
				(set-right (get-parent node) y)))
		(set-left y node)
		(set-parent node y)
		(set-max y (get-max node))
		(update-max node)))
(defn right-rotate [tree node]
	(let [y (get-left node)]
		(set-left node (get-right y))
		(if (not= (get-right y) (get-sentinel tree))
			(set-parent (get-right y) node))
		(set-parent y (get-parent node))
		(if (= (get-parent node) (get-sentinel tree))
			(set-root tree y)
			(if (= node (get-right (get-parent node)))
				(set-right (get-parent node) y)
				(set-left (get-parent node) y)))
		(set-right y node)
		(set-parent node y)
		(set-max y (get-max node))
		(update-max node))) 
(defn insert [tree interval value]
	(dosync
		(if (= (get-key (get-root tree)) nil)
			(let [root (get-root tree)] 
				(set-key root (low interval))
				(set-value root value)
				(set-interval root interval)
				(set-max root (high interval))
				(set-left root (get-sentinel tree))
				(set-right root (get-sentinel tree)))
			(let [low-key (low interval)
			      high-key (high interval)
			      node (interval-tree.core/node low-key value red nil nil nil interval high-key) 
				  y (ref (get-sentinel tree)) 
				  x (ref (get-root tree))]
				(loop []
					(if (not= @x (get-sentinel tree))
						(do
							(ref-set y @x)
							(if (> (high interval) (get-max @y))
								(set-max @y (high interval)))	
							(if (< (get-key node) (get-key @x))
								(ref-set x (get-left @x))
								(ref-set x (get-right @x)))
							(recur))))
				(do
					(set-parent node @y)
					(if (< (get-key node) (get-key @y))
						(set-left @y node)
						(set-right @y node))
					(set-left node (get-sentinel tree))
					(set-right node (get-sentinel tree))
					(set-color node red)
					)))))
(defn lookup-node [tree interval]
	(dosync
		(if (not= (get-key (get-root tree)) nil)
			(let [node-ref (ref (get-root tree))]
				(loop []
					(if (= @node-ref (get-sentinel tree))
						nil
						(if (and (== (low interval) (low (get-interval @node-ref)))
								 (== (high interval) (high (get-interval @node-ref))))
							@node-ref
							(do 
								(if (< (low interval) (get-key @node-ref))
									(ref-set node-ref (get-left @node-ref))
									(ref-set node-ref (get-right @node-ref)))
								(recur)))))))))
(defn lookup [tree interval]
	(let [node (lookup-node tree interval)]
		(if (not= node nil)
			(get-value node))))
(defn tree-minimum 
	([tree]
		(tree-minimum tree (get-root tree)))
	([tree node]
		(let [sentinel (get-sentinel tree)]
			(loop [min-node node]
				(let [left (get-left min-node)]
					(if (= left sentinel)
						min-node
						(recur left)))))))
(defn tree-maximum 
	([tree]
		(tree-maximum tree (get-root tree)))
	([tree node]
		(let [sentinel (get-sentinel tree)]
			(loop [max-node node]
				(let [right (get-right max-node)]
					(if (= right sentinel)
						max-node
						(recur right)))))))
(defn successor 
	([tree]
		(successor tree (get-root tree)))
	([tree node]
		(let [sentinel (get-sentinel tree) right (get-right node)]
			(if (not= right sentinel)
				(tree-minimum tree right)
				(let [x (ref node) y (ref (get-parent node))] 
					(while (and (not= @y sentinel)
					            (= @x (get-right @y))) 
						(dosync
							(ref-set x @y)
							(ref-set y (get-parent @y))))
					@y)))))
(defn inorder-walk 
	([tree]
		(inorder-walk tree (get-root tree)))
	([tree node]
		(if (not= node (get-sentinel tree))
			(do
				(inorder-walk tree (get-left node))
				(println (get-key node) " " (get-interval node))
				(inorder-walk tree (get-right node))))))
(defn point-query 
	([tree point] 
		(let [results (ref [])]
			(point-query tree (get-root tree) point results)
			@results))
	([tree node point results]
		(if (not= node (get-sentinel tree))
			(let [interval (get-interval node)]
				(if (<= point (get-max node))
					(point-query tree (get-left node) point results))
				(if (interval-tree.interval/contains interval point)
					(add-to-result node results))
				(if (>= point (low interval))
					(point-query tree (get-right node) point results))))))
(defn interval-query 
	([tree interval]
		(let [results (ref [])]
			(interval-query tree (get-root tree) interval results)
			@results))
	([tree node interval results]
		(if (not= node (get-sentinel tree))
			(let [node-interval (get-interval node)]
				(if (<= (high interval) (get-max node))
					(interval-query tree (get-left node) interval results))
				(if (interval-tree.interval/intersects? node-interval interval)
					(add-to-result node results))
				(if (>= (low interval) (low node-interval))
					(interval-query tree (get-right node) interval results))))))
(defn delete [tree interval]
	(dosync
		(let [z (ref (lookup-node tree interval))]
			(if (not= @z nil)
				(let [sentinel (get-sentinel tree) x (ref nil) y (ref nil) p (ref nil)]
					(if (or (= (get-left @z) sentinel)
							(= (get-right @z) sentinel))
						(ref-set y @z)
						(ref-set y (successor tree @z)))
					(ref-set p (get-parent @y))
					(if (not= (get-left @y) sentinel)
						(ref-set x (get-left @y))
						(ref-set x (get-right @y)))
					(set-parent @x (get-parent @y))
					(if (= (get-parent @y) sentinel)
						(set-root tree @x)
						(if (= @y (get-left (get-parent @y)))
							(set-left (get-parent @y) @x)
							(set-right (get-parent @y) @x)))
					(if (not= @y @z)
						(do
							(set-key @z (get-key @y))
							(set-interval @z (get-interval @y))
							(set-value @z (get-value @y))
							(update-max @z)
							(let [z-left (get-left @z) z-right (get-right @z)]
								(if (and (not= z-left nil) (not= z-left sentinel))
									(update-max z-left))
								(if (and (not= z-right nil) (not= z-right sentinel))
									(update-max z-right)))
							(let [parent (get-parent @z)]
								(if (and (not= parent nil) (not= parent sentinel))
									(update-max parent)))))
					(update-max-up-tree tree @p)
					))))