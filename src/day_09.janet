(use ./utils)

(def input (slurp "../input/day_09.txt"))

(def history-peg
  ~{:num  (number (* (? "-") :d+))
    :line (group (* :num (some (* (some " ") :num)) "\n"))
    :main (some :line)})

(defn calc-diffs [xs]
  (var diffs @[])
  (for i 1 (length xs)
    (let [a (xs i)
	  b (xs (dec i))]
      (array/push diffs (- a b))))
  diffs)

(defn gen-history-diffs [histories]
  (seq [hist :in histories]
    (var curr-hist hist)
    (def hist-diffs @[])
    (var i 0)
    (while (some |(not= $ 0) curr-hist)
      (array/push hist-diffs curr-hist) # beware mutation!
      (let [next-hist (calc-diffs curr-hist)]
	(set curr-hist next-hist))
      (if (> i 999999)
	(error "Too many iterations, might not terminate.")
	(+= i 1)))
    hist-diffs))


(defn predict-next [histories]
  (->> histories
       gen-history-diffs
       (map |(map last $))
       (map (fn [last-col]
	      (let [ns (reverse last-col)]
		(def next-col @[0])
		(each n ns
		  (let [next-n (last next-col)]
		    (array/push next-col (+ next-n n))))
		(last next-col))))
       sum))

(defn solve1 [input]
  (predict-next (peg/match history-peg input)))


(defn predict-prev [histories]
  (->> histories
       gen-history-diffs
       (map |(map first $))
       (map (fn [first-col]
	      (let [ns (reverse first-col)]
		(def next-col @[0])
		(each n ns
		  (let [next-n (last next-col)]
		    (array/push next-col (- n next-n))))
		(last next-col))))
       sum))

(defn solve2 [input]
  (predict-prev (peg/match history-peg input)))

(defn main [& args]
  (aoc-print 9 1 (solve1 input) 1904165718)
  (aoc-print 9 2 (solve2 input) 964))


(comment

  (def sample
    (string
      `
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`
      "\n"))

  (let [histories (peg/match history-peg sample)]
    (pp histories)
    (predict-next histories))

  (let [histories (peg/match history-peg sample)]
    (pp histories)
    (predict-prev histories))

  (calc-diffs @[10 13 16 21 30 45 68])

  )

