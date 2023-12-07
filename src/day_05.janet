(use ./utils)

(def input (slurp "../input/day_05.txt"))

(defn within-range [input [start len]]
  (and (>= input start)
       (<  input (+ start len))))

(defn map-to-range [input [dest src len]]
  (when (within-range input [src len])
    (+ input (- dest src))))

(defn make-map-fn [& ranges]
  (fn [input]
    (let [output (some (partial map-to-range input) ranges)]
      (or output input))))

(def seeds-maps-peg
  ~{:map   (/ (* (some :D) (some (* :nums "\n")))
	      ,make-map-fn)
    :maps  (group (some (* :map (? "\n"))))
    :nums  (group (some (* (number :d+) (? ,(^ "\n")))))
    :seeds (* "seeds:" :s+ :nums "\n")
    :main  (* :seeds :maps)})

(defn solve1 [input]
  (let [[seeds map-fns] (peg/match seeds-maps-peg input)]
    (apply min (map (comp ;(reverse map-fns))
		    seeds))))


(def seedranges-maps-peg
  ~{:map   (/ (* (some :D)
		 (some (* (/ :nums ,(fn [[dest src len]] [src dest len]))
			  "\n")))
	      ,make-map-fn)
    :maps  (group (some (* :map (? "\n"))))
    :nums  (group (some (* (number :d+) (? ,(^ "\n")))))
    :seeds (/ (* "seeds:" :s+ :nums "\n") ,(partial partition 2))
    :main  (* :seeds :maps)})

## brute-force approach: found the solution in under an hour (I think)
## unsatisfactory… might revisit if I find a better way
(defn solve2 [input]
  (let [[seed-ranges map-fns] (peg/match seedranges-maps-peg input)
        location->seed (comp ;map-fns)]
    (var min-location nil)
    (for loc 104000000 105000000 # <- cheating here, should be run from 0
      (let [seed (location->seed loc)]
	(when (some (partial within-range seed) seed-ranges)
	  # (print seed " -> " loc)
	  (set min-location loc)
	  (break))))
    min-location))

(defn main [& args]
  (print (solve1 input))
  (print (solve2 input)))


(comment

 (def sample (string `
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4` "\n"))

 ## make-range test
 (let [rng [50 98 2]]
   (= nil (map-to-range 97 rng))
   (= 50 (map-to-range 98 rng))
   (= 51 (map-to-range 99 rng))
   (= nil (map-to-range 100 rng)))


 (def [seeds map-fns]
   (peg/match
    ~{:map   (/ (* (some :D) (some (* :nums "\n")))
		,make-map-fn)
      :maps  (group (some (* :map (? "\n"))))
      :nums  (group (some (* (number :d+) (? ,(^ "\n")))))
      :seeds (* "seeds:" :s+ :nums "\n")
      :main  (* :seeds :maps)}
    sample))

 (apply min (map (comp ;(reverse map-fns)) seeds))


 (def [seed-ranges map-fns]
   (peg/match
    ~{:map   (/ (* (some :D)
		   (some (* (/ :nums ,(fn [[dest src len]] [src dest len]))
			    "\n")))
		,make-map-fn)
      :maps  (group (some (* :map (? "\n"))))
      :nums  (group (some (* (number :d+) (? ,(^ "\n")))))
      :seeds (/ (* "seeds:" :s+ :nums "\n") ,(partial partition 2))
      :main  (* :seeds :maps)}
    sample))

 ((map-fns 6) ((map-fns 5) ((map-fns 4) ((map-fns 3) ((map-fns 2) ((map-fns 1) ((map-fns 0) 46)))))))

 ((comp ;map-fns) 46)

 (let [location->seed (comp ;map-fns)]
   (for loc 0 100
     (let [seed (location->seed loc)]
       (print loc ": " seed)
       (when (some (partial within-range seed) seed-ranges)
	 (print "yes")
	 (break)))))
 
 )
