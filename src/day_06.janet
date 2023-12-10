(use ./utils)

(def input (slurp "../input/day_06.txt"))

(def records-peg
  ~{:nums (group (some (* (number :d+) :s+)))
	   :main (/ (* "Time:" :s+ :nums (? :s+) "Distance:" :s+ :nums)
		    ,|(apply map tuple $&))})


(defn count-wins [[total-ms record-mm]]
  (->> (range 1 total-ms)
       (filter (fn [hold-ms]
		 (let [dist-mm (* hold-ms (- total-ms hold-ms))]
		   (> dist-mm record-mm))))
       length))

(defn solve1 (input)
  (->> (peg/match records-peg input)
       first
       (map count-wins)
       product))

(defn solve2 (input)
  nil)

(defn main [& args]
  (print (solve1 input))
  (print (solve2 input)))



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

 )

