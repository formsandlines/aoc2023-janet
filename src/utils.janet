(import spork/math)

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

(defn merge-with* [f & maps]
  "Like merge-with, but with variable arity."
  (def r (array/slice maps))
  (when-let [m (array/pop r)]
    (case (length r)
      0 m
      1 (merge-with f m (array/pop r))
      (merge-with f m (apply merge-with* f r)))))

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

(defn congruent-modulo [a b n]
  # (= 0 (mod (- a b) n))  ## alternatively
  (= (mod a n) (mod b n)))

(defn euler-totient [n]
  (var num 0)
  (for a 1 (inc n)
    (when (= (math/gcd a n) 1)
      (+= num 1)))
  num)

(defn cart-product [xs ys]
  (seq [x :in xs
	y :in ys]
    [x y]))

(defn unique-pairs [xs &opt exclude-identity?]
  (seq [i :range [0 (length xs)]
	j :range [0 (length xs)]
	:when (if exclude-identity? (> j i) (>= j i))]
    [(xs i) (xs j)]))

(defn pairwise-coprime? [xs]
  (let [pairs (unique-pairs xs true)]
    (all |(= 1 (math/gcd ($ 0) ($ 1))) pairs)))

(defn lcm* [& xs]
  (case (length xs)
    0 (error "Must be at least two numbers.")
    1 (error "Must be at least two numbers.")
    2 (math/lcm (xs 0) (xs 1))
    (->> xs
	 (map math/factor)
	 (map frequencies)
	 (apply merge-with* max)
	 pairs
	 (map (partial apply math/pow))
	 product)))


(comment
  (let [xs @[12 18 24]]
    (->> xs
	 (map math/factor)
	 (map frequencies)
	 (apply merge-with* max)
	 pairs
	 (map (partial apply math/pow))
	 product))

  (lcm* 8 2 3 4)

  (merge-with*
    +
    @{:a 1 :b 4} @{:b 2 :a 5} @{:c 1 :a 7} @{:c 2 :b 1})

  (true? (pairwise-coprime? [5 7 11]))
  (false? (pairwise-coprime? [2 4 5]))

  (deep= @[[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]]
	 (cart-product (range 3) (range 3)))

  (all = (map euler-totient (range 1 21))
       [1 1 2 2 4 2 6 4 6 4
	10 4 12 6 8 8 16 6 18 8])
  
  (all true?
       [(true? (congruent-modulo 201 101 10))
	(false? (congruent-modulo 201 101 19))
	(true? (congruent-modulo 101 101 19))
	(true? (congruent-modulo 0 42 7))])

  (let [x @{:a 1 :b 2}
	y @{:a 3 :b 1 :c 4}]
    (merge-with + x y))
  # @{:a 4 :b 3 :c 4}

  (put-resolve @{:a 1 :b 2} :b 3 +)
  (put-resolve @{:a 1 :b 2} :c 3 +)

  (merge-entries-with + @[[:a 1] [:b 2] [:a 3]])

  (map-indexed tuple 1 ['a 'b 'c])

  
  (max-pair {:a 2 :b 5 :c 4})

  )
