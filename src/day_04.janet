(use ./utils)

(def input (slurp "../input/day_04.txt"))

(def cards-peg 
  ~{:nums  (group (some (* (number :d+) :s+)))
    :lines (group (* "Card" :s+ :d+ ":" :s+ :nums "|" :s+ :nums))
    :main  (some :lines)})

(defn count-winning-matches [[winnums mynums]]
  (let [matches (filter (partial has-value? mynums) winnums)]
    (length matches)))


(defn solve1 [input]
  (->> (peg/match cards-peg input)
       (map count-winning-matches)
       (map |(when (> $ 0) (math/pow 2 (dec $))))
       (filter (comp not nil?))
       sum))


(defn count-all-cards [matching-nums]
  (let [total (length matching-nums)
	cards (range total)
	copy-cards |(array/slice cards $0 $1)]
    ((fn rec [cards]
       (reduce (fn [acc i]
		 (let [num  (matching-nums i)
		       from (+ i 1)
		       to   (+ from num)]
		   (if (< from to total)
		     (+ 1 acc (rec (copy-cards from to)))
		     (+ 1 acc))))
	       0 cards))
     cards)))

(defn solve2 [input]
  (->> (peg/match cards-peg input)
       (map count-winning-matches)
       count-all-cards))

(defn main [& args]
  (print (solve1 input))
  (print (solve2 input)))




(comment

 ## found that with backticks, the last line-break will be ignored unless
 ## a character is added to the next line, so I add it manually to avoid
 ## parsing inconsistencies compared to the actual puzzle input
 (def sample
   (string `
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
` "\n"))
 
 (->> (peg/match cards-peg sample)
      (map count-winning-matches)
      count-all-cards)

 (def cards
   (peg/match
    ~{:nums  (group (some (* (number :d+) :s+)))
      :lines (group (* "Card " :d+ ":" :s+ :nums "|" :s+ :nums))
      :main  (some :lines)}
    sample))

 (->> cards
      (map (fn [[winnums mynums]]
	     (let [matches (filter (partial has-value? mynums)
				   winnums)
		   n       (length matches)]
	       (when (> n 0)
		 (math/pow 2 (dec n))))))
      (filter (comp not nil?))
      sum)

 )
