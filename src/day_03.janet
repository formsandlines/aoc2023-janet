(use ./utils)

(def input (slurp "../input/day_03.txt"))

(def positions-peg
  ~{:symb (/ (* ($) (<- ,(^ :s)))
	     ,(fn [i x] [i (= x "*")]))
    :num  (/ (* ($) (number :d+) ($))
	     ,(fn [i x j] [[i j] x]))
    :main (some (* (+ "." :num :symb) (? "\n")))})

(defn group-positions [positions]
  (reduce (fn [m x]
	    (if (indexed? (first x))
	      (update m :nums array/push x)
	      (update m :symbs array/push x)))
	  @{:nums @[] :symbs @[]}
	  positions))

(defn find-first-line-len [input] (+ 1 (peg/find "\n" input)))

(defn symbol-adjacent-to-num? [line-len row from to i]
  (let [i-row (div i line-len)
	i-col (% i line-len)
        adjacent? (and (< (math/abs (- i-row row)) 2)
		       (> i-col (- from 2))
		       (< i-col (+ to 1)))]
    adjacent?))


(defn filter-part-numbers [line-len {:nums nums :symbs symbs}]
  (let [symbol-idxs (map first symbs)]
    (filter (fn [[[i j] n]]
	      (let [row  (div i line-len)
		    from (% i line-len)
		    to   (% j line-len)]
		(some (partial symbol-adjacent-to-num? line-len row from to)
		      symbol-idxs)))
	    nums)))

(defn solve1 [input]
  (let [line-len (find-first-line-len input)]
    (->> (peg/match positions-peg input)
	 group-positions
	 (filter-part-numbers line-len)
	 (map last)
	 sum)))


(defn find-adjacent-nums [line-len idx nums]
  (map last
       (filter (fn [[[i j] n]]
		 (let [row  (div i line-len)
		       from (% i line-len)
		       to   (% j line-len)]
		   (symbol-adjacent-to-num? line-len row from to idx)))
	       nums)))

(defn collect-gear-nums [line-len {:nums nums :symbs symbs}]
  (let [symb-idxs (map first (filter last symbs))]
    (def gear-nums @[])
    (each idx symb-idxs
      (let [neighbors (find-adjacent-nums line-len idx nums)]
	(when (= 2 (length neighbors))
	  (array/push gear-nums neighbors))))
    gear-nums))

(def gear-ratio product)

(defn solve2 [input]
  (let [line-len (find-first-line-len input)]
    (->> (peg/match positions-peg input)
	 group-positions
	 (collect-gear-nums line-len)
	 (map gear-ratio)
	 sum)))


(defn main [& args]
  (aoc-print 3 1 (solve1 input) 519444)
  (aoc-print 3 2 (solve2 input) 74528807))



(comment

 (def sample
   `
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
 `)

 (def line-len-sample (find-first-line-len sample))

 (->> (peg/match positions-peg sample)
      group-positions
      (filter-part-numbers line-len-sample)
      (map last)
      sum)

 (->> (peg/match positions-peg sample)
      group-positions
      (collect-gear-nums line-len-sample)
      (map gear-ratio)
      sum)

 (def sample-pos
   (peg/match
    ~{:symb (* ($) ,(^ "\n"))
      :num  (/ (* ($) (number :d+) ($))
	       ,(fn [i x j] [[i j] x]))
      :main (some (* (+ "." :num :symb) (? "\n")))}
    sample))

 )
