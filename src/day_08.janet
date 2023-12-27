(use ./utils)

(def input (slurp "../input/day_08.txt"))

(def [L R A Z] (string/bytes "LRAZ"))

(def wayfinding-peg
  ~{:loc   (<- (3 :w))
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


(defn last-char? [c str] (= (last str) c))

(defn traverse-signposts-parallel [[dirs signposts] locs]
  (let [get-next-loc (fn [loc dir]
		       (let [signp (get signposts loc)]
			 (get signp dir)))]
    (var step 0)
    (while true
      (let [dir (get dirs (mod step (length dirs)))]
	(eachk i locs
	  (update locs i get-next-loc dir))
	(cond
	  (true? (all (partial last-char? Z) locs)) (break)
	  (> step 999999) (error "Too many steps, search might not terminate.")
	  (+= step 1))))
    (inc step)))

(defn solve2-bruteforce [input]
  ## Does not work - too many steps! See `solve2` for the actual solution.
  (let [[dirs signposts] (peg/match wayfinding-peg input)
	all-locs   (keys signposts)
	start-locs (filter (partial last-char? A) all-locs)]
    (traverse-signposts-parallel [dirs signposts] start-locs)))


(defn collect-cycles [dirs signposts init-locs]
  ## takes a few minutes, I guess it could be optimized
  (let [get-next-loc  (fn [loc dir] (let [signp (get signposts loc)]
				      (get signp dir)))
	get-dir-index (fn [step] (mod step (length dirs)))
	locs    (array/slice init-locs)
	cycles  (array/new-filled (length locs))
	threads (seq [i :range [0 (length locs)]] @[{:loc (locs i) :dir 0}])]
    (var step 0)
    (while (some nil? cycles)
      (eachk i locs
	(when (nil? (cycles i))
	  (update locs i get-next-loc (get dirs (get-dir-index step)))
	  (let [next-loc (locs i)
		next-dir (get-dir-index (inc step))
		thread   (threads i)
		cycle-start? (fn [{:loc l :dir d}] (and (= next-loc l)
							(= next-dir d)))]
	    (when-let [cyc-from (find-index cycle-start? thread)]
	      (put cycles i {:pre (array/slice thread 0 cyc-from)
			     :cyc (array/slice thread cyc-from
					       (inc step))}))
	    (array/push thread {:loc next-loc
				:dir next-dir}))))
      (if (> step 999999)
	(error "Too many steps, search might not terminate.")
	(+= step 1)))
    cycles))

(defn collect-z-idxs [cyc]
  (let [arr @[]]
    (eachk i cyc
      (when (last-char? Z ((cyc i) :loc))
	(array/push arr i)))
    arr))

(defn solve2 [input]
  ## see my comments on the bottom of the file for a breakdown
  (let [[dirs signposts] (peg/match wayfinding-peg input)
	all-locs   (keys signposts)
	start-locs (filter (partial last-char? A) all-locs)
	cycles     (collect-cycles dirs signposts start-locs)]
    (->> cycles
	 (map (fn [{:pre pre :cyc cyc}] {:z-offsets (collect-z-idxs cyc)
					 :cyc-len   (length cyc)
					 :start     (length pre)}))
	 (map |($ :cyc-len))
	 (apply lcm*))))


(defn main [& args]
  (aoc-print 8 1 (solve1 input) 12737)
  (aoc-print 8 2 (solve2 input) 9064949303801))


(comment

  (def p1-sample1
    (string `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
` "\n"))

  (def p1-sample2
    (string `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
` "\n"))

  (let [[dirs signposts] (peg/match wayfinding-peg p1-sample2)
	from-loc "AAA"
	to-loc "ZZZ"]
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
    (inc step))

  (let [[dirs signposts] (peg/match wayfinding-peg p1-sample1)]
    (get (get signposts "AAA") (get "LR" 1)))


  (def p2-sample
    (string `LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
` "\n"))

  ## For part 2, brute-forcing the example is fine:
  (let [[dirs signposts] (peg/match wayfinding-peg p2-sample)
	all-locs   (keys signposts)
	start-locs (filter (partial last-char? A) all-locs)]
    (traverse-signposts-parallel [dirs signposts] start-locs))
  #=> 6
  
  ## With the actual input, it fails as the number of steps required
  ## is too big, so there has to be a mathematical solution to predict
  ## the number of steps until all names ending in 'Z' align.
  
  ## This suggested that repeating patterns may occur in each path,
  ## so let’s collect these cycles for further analysis:
  (let [[dirs signposts] (peg/match wayfinding-peg p2-sample)
	all-locs   (keys signposts)
	start-locs (filter (partial last-char? A) all-locs)
	cycles     (collect-cycles dirs signposts start-locs)]
    ## Each cycle can begin at any step and a name ending in 'Z'
    ## can appear anywhere in a cycle, possibly multiple times,
    ## so all of this information has to be be included:
    (map (fn [{:pre pre :cyc cyc}] {:z-offsets (collect-z-idxs cyc)
				    :cyc-len   (length cyc)
				    :start     (length pre)})
	 cycles))

  ## cycles collected from the example:
  (def cyc-data-sample @[{:cyc-len 2 :start 1 :z-offsets @[1]}
			 {:cyc-len 6 :start 1 :z-offsets @[2 5]}])
  
  (def cycles-sample (map |($ :cyc-len) cyc-data-sample))

  ## this yields the following system of congruences to solve,
  ## where x ≡ start + z-offset (mod cyc-len):
  ## a)  x ≡ 1+1 (mod 2)
  ## b1) x ≡ 1+2 (mod 6)
  ## b2) x ≡ 1+5 (mod 6)

  ## a)  ? . Z . Z . Z . Z . …
  ## b1) . . . Z . . . . . Z … (clearly no alignment)
  ## b2) ? . . . . . Z . . . … (aligns at 6)
  ##     0 1 2 3 4 5 6 7 8 9 …
  
  ## the offsets for cycles a and b2 are exactly equal to their lengths:
  (deep= cycles-sample
	 (map |(+ ($ :start) (last ($ :z-offsets))) cyc-data-sample))
  #=> true

  ## so, cycle alignment is easily found by their least common multiple:
  ## x = lcm(mod a, mod b2)
  (apply lcm* cycles-sample) #=> 6


  ## cycles collected from the actual input:
  (def cyc-data @[{:cyc-len 12737 :start 2 :z-offsets @[12735]}
		  {:cyc-len 18157 :start 2 :z-offsets @[18155]}
		  {:cyc-len 15989 :start 3 :z-offsets @[15986]}
		  {:cyc-len 11653 :start 2 :z-offsets @[11651]}
		  {:cyc-len 21409 :start 3 :z-offsets @[21406]}
		  {:cyc-len 14363 :start 5 :z-offsets @[14358]}])

  (def cycles (map |($ :cyc-len) cyc-data))

  ## system of congruences to solve:
  ## a) x ≡ 2 + 12735 (mod 12737)
  ## b) x ≡ 2 + 18155 (mod 18157)
  ## c) x ≡ 3 + 15986 (mod 15989)
  ## d) x ≡ 2 + 11651 (mod 11653)
  ## e) x ≡ 3 + 21406 (mod 21409)
  ## f) x ≡ 5 + 14358 (mod 14363)

  ## this time, each cycle contains only one name ending in 'Z',
  ## so we don’t have to account for alternative offsets

  ## once again, all offsets are equal to the corresponding cycle lengths:
  (deep= cycles (map |(+ ($ :start) (first ($ :z-offsets))) cyc-data))
  #=> true (how convenient :))

  ## therefore, the same method can be used to find the alignment:
  (apply lcm* cycles) #=> 9064949303801

  )

