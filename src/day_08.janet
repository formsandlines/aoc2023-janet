(use ./utils)

(def input (slurp "../input/day_08.txt"))

(def [L R] (string/bytes "LR"))

(def wayfinding-peg
  ~{:loc   (<- (3 :a))
    :ways  (/ (group (* "(" :loc ", " :loc ")"))
	      ,|{L ($ 0) R ($ 1)})
    :assoc (group (* :loc " = " :ways))
    :map   (/ (group (some (* :assoc (? "\n"))))
	      ,from-pairs)
    :dirs  (<- (some (set "LR")))
    :main  (* :dirs :s+ :map)})

(defn traverse-signposts [[dirs signposts] from-loc to-loc]
  (var step 0)
  (var loc from-loc)
  (while true
    (let [signp (get signposts loc)
	  dir   (get dirs (mod step (length dirs)))
	  next-loc (get signp dir)]
      (cond
	(= to-loc next-loc) (break)
	(> step 99999) (error "Too many steps, search might not terminate.")
	(set loc next-loc)))
    (+= step 1))
  (inc step)) # add 0’th step

(defn solve1 [input]
  (traverse-signposts (peg/match wayfinding-peg input) "AAA" "ZZZ"))

(defn solve2 [input]
  nil)


(defn main [& args]
  (aoc-print 8 1 (solve1 input) 12737)
  (aoc-print 8 2 (solve2 input) "?"))


(comment

  (def sample1
    (string `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
` "\n"))

  (def sample2
    (string `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
` "\n"))


  (let [[dirs signposts] (peg/match wayfinding-peg sample2)]
    (var step 0)
    (var loc "AAA")
    (while true
      (let [signp (get signposts loc)
	    dir   (get dirs (mod step (length dirs)))
	    next-loc (get signp dir)]
	(cond
	  (= "ZZZ" next-loc) (break)
	  (> step 999) (error "Too many ways.")
	  (set loc next-loc)))
      (+= step 1))
    (inc step))

  (let [[dirs signposts] (peg/match wayfinding-peg sample1)]
    (get (get signposts "AAA") (get "LR" 1)))

  )

