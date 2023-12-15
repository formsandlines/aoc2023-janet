(defn aoc-print [day part result solution]
  (print "✨ Day " day ", part " part)
  (print "   Result: " result)
  (if (= result solution)
    (print "   Correct, yay!")
    (print "   Incorrect, oh noes! Expected: " solution))
  (print))

(defn table-from-entries [entries]
  (table ;(flatten entries)))

(defn put-resolve [tb k v f]
  "Like `put`, but resolves conflicts with given function, if a key is already present in the table."
  (if-let [v-old (tb k)]
    (put tb k (f v-old v))
    (put tb k v)))

(defn merge-with
  "Merge tables, resolving conflicts with given function, like in Clojure."
  [f d1 d2]
  (eachp [k v2] d2
    (put-resolve d1 k v2 f))
  d1)

(defn merge-entries-with
  "Like `merge-with`, but for a single array of table-entries."
  [f kvs]
  (reduce (fn [m [k v2]]
	    (put-resolve m k v2 f))
	  @{}
	  kvs))

(defn ^ [pat]
  ~(* (! ,pat) 1))

(defn map-indexed
  "`map` with an additional index (first arg), like in Clojure.
Unlike in Clojure, takes an additional integer `from` as the starting index."
  [f from ind]
  (let [to (+ from (length ind))]
    (map f (range from to) ind)))

(defn max-pair
  "Returns the (first) key-value pair that matches the result of `max` applied
to all values in the associative data-structure."
  [dict]
  (let [pairs (pairs dict)]
    (var max-pair (first pairs))
    (each [k v] pairs
      (when (> v (max-pair 1))
	(set max-pair [k v])))
    max-pair))

(defn dissoc [tab k]
  "Returns a new table without the given key, like in Clojure (but much less
efficient)."
  (do
    (var t (table/clone tab))
    (put t k nil)
    t))


(comment
  (let [x @{:a 1 :b 2}
	y @{:a 3 :b 1 :c 4}]
    (merge-with + x y))
  # @{:a 4 :b 3 :c 4}

  (put-resolve @{:a 1 :b 2} :b 3 +)
  (put-resolve @{:a 1 :b 2} :c 3 +)

  (merge-entries-with + @[[:a 1] [:b 2] [:a 3]])

  (map-indexed tuple 1 ['a 'b 'c])

  
  (max-pair {:a 2 :b 5 :c 4})

  (let [ds @{:a 2 :b 5 :c 4}]
    (do (var t (table/clone ds))
      (put t :b nil)
      t)
    ds)

  )
