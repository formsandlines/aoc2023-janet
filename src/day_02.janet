(use ./utils)

(def input (slurp "../input/day_02.txt"))

(def colors ["red" "green" "blue"])

(defn parse-stats [input f]
  (peg/match
   ~{:sample  (/ (group (* (number :d+) :s (/ (<- (+ ,;colors)) ,keyword)))
		 ,(fn [[v k]] [k v]))
     :samples (/ (group (some (* :sample (? (* (set ";,") :s)))))
		 ,(partial merge-entries-with f))
     :id   (* "Game " (number :d+) ": ")
     :line (group (* :id :samples))
     :main (some (* :line "\n"))}
   input))

(defn solve1 [input]
  (->> (parse-stats input max)
       (filter (fn [[k v]]
		 (and (<= (v :red) 12)
		      (<= (v :green) 13)
		      (<= (v :blue) 14))))
       (map first)
       sum))

(defn solve2 [input]
  (->> (parse-stats input max)
       (map (fn [[_ v]] (product (values v))))
       sum))


(defn main [& args]
  (aoc-print 2 1 (solve1 input) 2727)
  (aoc-print 2 2 (solve2 input) 56580))



(comment
 (def ex1
   `
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
 `)

 (solve1 ex1)
 (solve2 ex1)

 
 (def ex1-sums {1 {:blue 9 :red 5 :green 4}
		2 {:blue 6 :red 1 :green 6}
		3 {:blue 11 :red 25 :green 26}
		4 {:blue 21 :red 23 :green 7}
		5 {:blue 3 :red 7 :green 5}})

 (->> (pairs ex1-sums)
      (filter (fn [[k v]]
		(and (<= (v :red) 12)
		     (<= (v :green) 13)
		     (<= (v :blue) 14))))
      (map first)
      sum)

 )

