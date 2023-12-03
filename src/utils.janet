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


(comment
 (let [x @{:a 1 :b 2}
       y @{:a 3 :b 1 :c 4}]
   (merge-with + x y))
 # @{:a 4 :b 3 :c 4}

 (put-resolve @{:a 1 :b 2} :b 3 +)
 (put-resolve @{:a 1 :b 2} :c 3 +)

 (merge-entries-with + @[[:a 1] [:b 2] [:a 3]])

 )
