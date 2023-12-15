(use ./utils)

(def input (slurp "../input/day_06.txt"))

## Can this be optimized with math, avoiding the need of a sequence?
(defn count-wins [[total-ms record-mm]]
  (->> (range 1 total-ms)
       (filter (fn [hold-ms]
		 (let [dist-mm (* hold-ms (- total-ms hold-ms))]
		   (> dist-mm record-mm))))
       length))


(def records-peg
  ~{:nums (group (some (* (number :d+) :s+)))
    :main (/ (* "Time:" :s+ :nums (? :s+) "Distance:" :s+ :nums)
	     ,|(apply map tuple $&))})

(defn solve1 (input)
  (->> (peg/match records-peg input)
       first
       (map count-wins)
       product))


(def single-record-peg
  ~{:num  (/ (% (some (* (<- :d+) :s+))) ,scan-number)
    :main (* "Time:" :s+ :num (? :s+) "Distance:" :s+ :num)})

(defn solve2 (input)
  (count-wins (peg/match single-record-peg input)))


(defn main [& args]
  (aoc-print 6 1 (solve1 input) 293046)
  (aoc-print 6 2 (solve2 input) 35150181))



(comment

 (def sample
   (string `
Time:      7  15   30
Distance:  9  40  200
` "\n"))

 (->> (peg/match records-peg sample)
      first
      (map count-wins)
      product)

 (->> (peg/match single-record-peg sample)
      count-wins)

 )

