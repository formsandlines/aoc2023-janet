(use ./utils)

(def input (slurp "../input/day_01.txt"))

(def digit-words
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def word->digit
  (zipcoll digit-words (range 1 10)))

## part 2 inspired by:
## https://github.com/ianthehenry/aoc-2023/blob/master/01.janet
## since I had trouble to avoid capturing the whole word
(defn make-grammar [words?]
  ~{:digit (+ (number :d)
	      ,;(if words?
		  (map (fn [s] ~(/ (if ,s 1) ,(word->digit s)))
		       digit-words)
		  '()))
    :line  (/ (group (some (+ :digit (* (! "\n") 1))))
	      ,|(when (not (empty? $))
		  (+ (* 10 (first $)) (last $))))
    :main  (/ (some (* :line "\n")) ,+)})

(defn solve1 [input]
  (first (peg/match (make-grammar false) input)))

(defn solve2 [input]
  (first (peg/match (make-grammar true) input)))


(defn main [& args]
  (aoc-print 1 1 (solve1 input) 54990)
  (aoc-print 1 2 (solve2 input) 54473))



(comment

 (peg/match
  (make-grammar true)
  `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
 `)

 (peg/match
  (make-grammar false)
  `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
 `)

 (peg/match
  ~{:digit (+ (number :d)
	      ,;(map (fn [s] ~(/ (if ,s 1) ,(word->digit s)))
		     digit-words))
    :line  (/ (group (some (+ :digit (* (! "\n") 1))))
	      ,|(when (not (empty? $))
		  (+ (* 10 (first $)) (last $))))
    :main  (some (* :line "\n"))}
  `mnbrf3fourfpbrdgltf2xbmbmrbjltdxbklsixoneightq
ssfzbthree8twoneqqn
 `)
 )

