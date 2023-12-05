(use ./utils)

(def input (slurp "../input/day_03.txt"))

(def positions-peg
  ~{:char (* ($) ,(^ "\n"))
    :num  (/ (* ($) (number :d+) ($))
	     ,(fn [i x j] [[i j] x]))
    :main (some (* (+ "." :num :char) (? "\n")))})

(defn group-positions [positions]
  (reduce (fn [m x]
	    (if (indexed? x)
	      (update m :nums array/push x)
	      (update m :chars array/push x)))
	  @{:nums @[] :chars @[]}
	  positions))

(defn find-first-line-len [input] (+ 1 (peg/find "\n" input)))

(defn index-adjacent-to-str? [line-len row from to i]
  (let [i-row (div i line-len)
	i-col (% i line-len)
        adjacent? (and (< (math/abs (- i-row row)) 2)
		       (> i-col (- from 2))
		       (< i-col (+ to 1)))]
    # (print (string row ": " from "-" to "  " i-row ": " i-col " " adjacent?))
    adjacent?))

(defn filter-part-numbers [line-len {:nums nums :chars chars}]
  (filter (fn [[[i j] n]]
	    (let [row  (div i line-len)
		  from (% i line-len)
		  to   (% j line-len)]
	      (some (partial index-adjacent-to-str? line-len row from to)
		    chars)))
	  nums))

(defn solve1 [input]
  (let [line-len (find-first-line-len input)]
    (->> (peg/match positions-peg input)
	 group-positions
	 ((partial filter-part-numbers line-len))
	 (map last)
	 sum)))

(defn solve2 [input]
  nil)

(defn main [& args]
  (print (solve1 input))
  (print (solve2 input)))



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

 (def sample
   `
467..114..
....$.....
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
      ((partial filter-part-numbers line-len-sample))
      (map last)
      # sum
      )



 (def sample-pos
   (peg/match
    ~{:char (* ($) ,(^ "\n"))
      :num  (/ (* ($) (number :d+) ($))
	       ,(fn [i x j] [[i j] x]))
      :main (some (* (+ "." :num :char) (? "\n")))}
    sample))


 (->> sample-pos
      (reduce (fn [m x]
		(if (indexed? x)
		  (update m :nums array/push x)
		  (update m :chars array/push x)))
	      @{:nums @[] :chars @[]})
      ((fn [{:nums nums :chars chars}]
	 (filter (fn [[[i j] n]]
		   (let [row  (div i line-len-sample)
			 from (% i line-len-sample)
			 to   (% j line-len-sample)]
		     (some (partial in-bounds? row from to)
			   chars)))
		 nums)))
      (map last)
      sum)

 (peg/match
  '(some (* ($) (<- 1)))
  `hallo
welt`)

 (peg/match
  ~(some (* (/ (* ($) (<- :w+) ($))
	       ,(fn [i x j] [[i j] x]))
	    (? :s)))
  `hallo welt`)

 )
