(use ./utils)

(def input (slurp "../input/day_07.txt"))

(defn make-card-lbl->strength [card-lbls]
  (zipcoll card-lbls (reverse (range (length card-lbls)))))

(defn hands-comparator [{:type a-type :hand a-hand}
			{:type b-type :hand b-hand}]
  (if (= a-type b-type)
    (do (var ak nil) (var bk nil) (var res 0)
      (while true
	(set ak (next a-hand ak))
	(set bk (next b-hand bk))
	(when (or (nil? ak) (nil? bk)) (break))
	(set res (compare (in a-hand ak) (in b-hand bk)))
	(when (not= 0 res) (break)))
      res)
    (compare a-type b-type)))

(defn sort-hands [hands-data]
  (sort hands-data compare<))

(defn make-hand [type-fn [hand bid]]
  @{:bid bid :hand hand :type (type-fn hand)
    :compare hands-comparator})

(defn make-hands-peg [card-lbls]
  ~{:bid  (number :d+)
    :hand (group (5 (/ (<- (set ,(string/join card-lbls "")))
		       ,(make-card-lbl->strength card-lbls))))
    :line (group (* :hand :s+ :bid "\n"))
    :main (some :line)})

(defn freqs->type [freqs]
  (match (sort (values freqs) >)
    [5]   6 # five-of-a-kind
    [4]   5 # four-of-a-kind
    [3 2] 4 # full-house
    [3]   3 # three-of-a-kind
    [2 2] 2 # two-pair
    [2]   1 # one-pair
    [1]   0 # high-card
    _ (error "Incorrect hand")))


(def card-lbls-p1
  ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(defn hand-type-p1 [hand]
  (freqs->type (frequencies hand)))

(defn solve1 [input]
  (->> (peg/match (make-hands-peg card-lbls-p1) input)
       (map (partial make-hand hand-type-p1))
       sort-hands
       (map-indexed |(* ($1 :bid) $0) 1)
       sum))


(def card-lbls-p2
  (let [i (index-of "J" card-lbls-p1)]
    (array/push
      (array/remove (array/slice card-lbls-p1) i) "J")))

(defn hand-type-p2 [hand]
  (let [freqs (frequencies hand)]
    (when-let [jokers-num (when (> (length freqs) 1)
			    (freqs 0))]
      (do
	(put freqs 0 nil)
	(let [max-k (first (max-pair freqs))]
	  (update freqs max-k |(+ jokers-num $)))))
    (freqs->type freqs)))

(defn solve2 [input]
  (->> (peg/match (make-hands-peg card-lbls-p2) input)
       (map (partial make-hand hand-type-p2))
       sort-hands
       (map-indexed |(* ($1 :bid) $0) 1)
       sum))


(defn main [& args]
  (print (solve1 input))
  (print (solve2 input)))


(comment

  (def sample
    (string `
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
` "\n"))

  (sort (map (partial make-hand hand-type-p1)
	     @[[@[1 1 8 1 1] 765]
	       [@[1 1 7 1 1] 684]])
	compare<)


  (each hand (->> (peg/match (make-hands-peg card-lbls-p2) sample)
		  (map (partial make-hand hand-type-p2)))
    (pp hand))

  (->> (peg/match (make-hands-peg card-lbls-p2) sample)
       (map (partial make-hand hand-type-p2))
       sort-hands
       (map-indexed |(* ($1 :bid) $0) 1)
       sum)
  
  )
