(use ./utils)

(def input (slurp "../input/day_07.txt"))

(def card-lbls
  ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(def card-lbl->strength
  (zipcoll card-lbls (reverse (range (length card-lbls)))))

(defn hand-type [hand]
  (let [freq (frequencies hand)
        nums (sort (values freq) >)]
    # (pp freq)
    # (pp nums)
    (match nums
      [5]   6 # five-of-a-kind
      [4]   5 # four-of-a-kind
      [3 2] 4 # full-house
      [3]   3 # three-of-a-kind
      [2 2] 2 # two-pair
      [2]   1 # one-pair
      [1]   0 # high-card
      _ (error "Incorrect hand"))))

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

(defn make-hand [[hand bid]]
  @{:bid bid :hand hand :type (hand-type hand)
    :compare hands-comparator})


(def hands-peg
  ~{:bid  (number :d+)
    :hand (group (5 (/ (<- (set ,(string/join card-lbls "")))
		       ,card-lbl->strength)))
    :line (group (* :hand :s+ :bid "\n"))
    :main (some :line)})

(defn solve1 [input]
  (->> (peg/match hands-peg input)
       (map make-hand)
       sort-hands
       (map-indexed |(* ($1 :bid) $0) 1)
       sum))


(defn solve2 [input]
  )


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

  (+ (* 5 483)
     (* 4 684)
     (* 3 28)
     (* 2 220)
     (* 1 765))

  (sort (map make-hand @[[@[1 1 8 1 1] 765]
			 [@[1 1 7 1 1] 684]])
	compare<)

  (->> (peg/match
	 ~{:bid  (number :d+)
	   :hand (group (5 (/ (<- (set ,(string/join card-lbls "")))
			      ,card-lbl->strength)))
	   :line (group (* :hand :s+ :bid "\n"))
	   :main (some :line)}
	 sample)
       (map make-hand)
       sort-hands
       (map-indexed |(* ($1 :bid) $0) 1)
       sum)
  
  (let [ds [:a :b :c]]
    (var k nil)
    (while true
      (set k (next ds k))
      (if (= nil k) (break))
      (print (in ds k))))

  )
