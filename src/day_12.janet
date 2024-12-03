(use ./utils)

(def input (slurp "../input/day_12.txt"))

(def [hash dot qmark] [35 46 63])

(def spring-records-peg
  ~{:group-sizes (group (some (+ (number :d+) ",")))
    :spring-cond (<- (some (set "?#.")))
    :record (group (* :spring-cond :s+ :group-sizes "\n"))
    :main (some :record)})

(defn matches-conditions?
  [[cand cond]]
  (case [cand cond]
    [hash hash]  true
    [dot  dot]   true
    [hash qmark] true
    [dot  qmark] true
    false))

(defn match-candidate
  [candidate conditions &opt from to]
  (default from 0)
  (default to (length candidate))
  (var match? true)
  (for i from to
    (when (not (matches-conditions? [(candidate i) (conditions i)]))
      (set match? false)
      (break)))
  match?)

(defn count-arrangements
  [[spring-conds group-sizes]]
  (let [len (length spring-conds)]
    ((fn rec [sizes candidate min]
       (let [size  (first sizes)
	     bound (+ (sum sizes) (dec (length sizes)))
	     max   (inc (- len bound))
	     group (string/repeat "#" size)]
	 (sum
	   ## iterate over all possible positions
	   (seq [i :range [min max]
		 ## recursively build the candidate for that position
		 ## and match it against the spring conditions
		 :let [next-part (string (when (> min 0) ".")
					 (string/repeat "." (- i min))
					 group)
		       fill (if (= 1 (length sizes))
			      (string/repeat "." (- len (+ (length candidate)
							   (length next-part))))
			      "")
		       next-cand (string candidate next-part fill)
		       match-start (if (> min 0) (- min 1) min)
		       match-end (+ match-start (length next-part)
				    (length fill))
		       matches? (match-candidate next-cand spring-conds
						 match-start match-end)]]
	     (if matches?
	       (if (= 1 (length sizes))
		 1
		 (rec (slice sizes 1) next-cand (+ i size 1)))
	       0)))))
      group-sizes "" 0)))

(defn solve1 [input]
  (->> (peg/match spring-records-peg input)
       (map count-arrangements)
       sum))

(defn unfold-record
  [[conds groups]]
  (let [copies 5
	new-conds (buffer conds)
	new-groups (array ;groups)]
    (for i 0 (dec copies)
      (buffer/push-string new-conds "?" conds)
      (array/concat new-groups groups))
    [(string new-conds) new-groups]))

(defn solve2 [input]
  (->> (peg/match spring-records-peg input)
       # (drop 5)
       # (take 1)
       (map unfold-record)
       (map count-arrangements)
       sum
       #
       ))

(defn pp-all [x] (printf "%j" x))

(defn main [& args]
  (aoc-print 12 1 (solve1 input) 7857)
  (aoc-print 12 2 (solve2 input) "???"))


(def ex1 ``???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
 ``) # 1 4 1 1 4 10

(def ex2 ``???..??#?#?????????. 1,3,10
..????????#???????? 10,3
???.?????. 2,1
?#?????.??. 2,1
??.#.?..?? 1,2
.??##?#????.?? 5,1
#??????????.#.?..?#. 9,1,1,1,1
###.?#???#. 3,6
?.#???#?????? 1,7,1
???????#.. 4,2
??..#????###?????# 2,2,1,7,1
?????#???????????#?# 1,4,1,2,1,1
 ``) # 3 10 13 11 1 9 1 1 4 2 1 106

(def ex3 ``???#?????? 1,1,1
?????#???????????#?# 1,4,1,2,1,1
???#??.???????#? 4,1,1,1,2
????????.??.##?? 2,1,1,2
???#???.??? 3,1,1
 ``) # ×5: 49142208 ? 66659296 ? 12578628


(comment
  # Optimized new attempt:

  (->> (take 2 (peg/match spring-records-peg ex1))
       (map unfold-record)
       (map count-arrangements)
       # sum
       #
       )

  (->> (array ((peg/match spring-records-peg ex1) 3))
       (map unfold-record)
       (map count-arrangements)
       # sum
       #
       )
  
  (string/repeat "???.###" 5)

  (string/join (repeat 5 "???.###"))

  (let [copies 5
	conds "???.###"
	new-conds (buffer conds)
	groups [1 1 3]
	new-groups (array ;groups)]
    (for i 0 (dec copies)
      (buffer/push-string new-conds "?" conds)
      (array/concat new-groups groups))
    [(string new-conds) new-groups])

  (defn last-index-of
    [s x]
    (var index nil)
    (loop [i :down-to [(dec (length s)) 0]]
      (when (= (s i) dot)
	(set index i)
	(break)))
    index)

  (seq [[spring-conds group-sizes] :in (->> (peg/match spring-records-peg
						       ex3)
					    (map unfold-record)
					    (drop 3)
					    (take 1))
	:let [len (length spring-conds)]]
    # (pp group-sizes)
    # (print spring-conds)
    # (print "candidates:")
    ((fn rec [sizes candidate min]
       (let [size  (first sizes)
	     bound (+ (sum sizes) (dec (length sizes)))
	     max   (inc (- len bound))
	     group (string/repeat "#" size)]
	 (var count 0)
	 (var total 0)
	 ## iterate over all possible positions
	 (forv i min max
	   (let [dot-index (last-index-of
			     (string/slice spring-conds i (+ i size)) dot)]
	     (if dot-index
	       ## if a '.' is found anywhere, skip that dot (i is mutable)
	       (set i (+ i dot-index))
	       ## otherwise, build the candidate for the current position
	       (let [next-part (string (when (> min 0) ".")
				       (string/repeat "." (- i min))
				       group)
		     fill (if (= 1 (length sizes))
			    (string/repeat "." (- len (+ (length candidate)
							 (length next-part))))
			    "")
		     next-cand (string candidate next-part fill)
		     match-start (if (> min 0) (- min 1) min)
		     match-end (+ match-start (length next-part)
				  (length fill))]
		 # (print next-cand)
		 (when (match-candidate next-cand spring-conds
					match-start match-end)
		   (if (= 1 (length sizes))
		     (do
		       # (print next-cand)
		       (++ count)
		       (when (and (> count 10000000)
				  (< count 14000000))
			 (spit "./out.txt" (string next-cand "\n") :a))
		       (++ total))
		     (set total
			  (+ total
			     (rec (slice sizes 1) next-cand (+ i size 1))))))
		 ## if there is a # here, we must not leave it behind
		 (when (= hash (spring-conds i))
		   # (print "break!")
		   (break))))))
	 total))
      group-sizes "" 0))

  
  #
  )


(comment
  ## last attempt

  (seq [[spring-conds group-sizes] :in (->> (peg/match spring-records-peg
						       ex3)
					    (map unfold-record)
					    (drop 0)
					    (take 1))
	:let [len (length spring-conds)]]
    # (pp group-sizes)
    # (print spring-conds)
    # (print "candidates:")
    ((fn rec [sizes candidate min]
       (let [size  (first sizes)
	     bound (+ (sum sizes) (dec (length sizes)))
	     max   (inc (- len bound))
	     group (string/repeat "#" size)]
	 # (printf "bounds: [%q %q]" min max)
	 (sum
	   ## iterate over all possible positions
	   (seq [i :range [min max]
		 ## build the candidate for that position
		 :let [next-part (string (when (> min 0) ".")
					 (string/repeat "." (- i min))
					 group)
		       fill (if (= 1 (length sizes))
			      (string/repeat "." (- len (+ (length candidate)
							   (length next-part))))
			      "")
		       next-cand (string candidate next-part fill)
		       match-start (if (> min 0) (- min 1) min)
		       match-end (+ match-start (length next-part)
				    (length fill))
		       matches? (match-candidate next-cand spring-conds
						 match-start match-end)]]
	     # (print next-cand)
	     (if matches?
	       (if (= 1 (length sizes))
		 (do
		   (print next-cand)
		   1)
		 (rec (slice sizes 1) next-cand (+ i size 1)))
	       0)))))
      group-sizes "" 0))
  #
  )

(comment
  ## New attempt:
  (match-candidate "..#.##..#"
		   "?.??##??#")

  # ???.### 1,1,3

  (defn match-candidate
    [candidate conditions]
    (if (= (length candidate)
	   (length conditions))
      (all matches-conditions? (map tuple candidate conditions))
      (error "Candidate length does not match conditions length.")))
  
  (seq [[spring-conds group-sizes] :in (peg/match spring-records-peg
						  ex2)
	:let [len (length spring-conds)]]
    (pp group-sizes)
    (print spring-conds)
    (print "candidates:")
    ((fn rec [sizes candidate min]
       (let [size  (first sizes)
	     bound (+ (sum sizes) (dec (length sizes)))
	     max   (inc (- len bound))]
	 # (printf "bounds: [%q %q]" min max)
	 (sum
	   ## iterate over all possible positions
	   (seq [i :range [min max]
		 ## build the candidate for that position
		 :let [next-cand (string candidate
					 (when (> min 0) ".")
					 (string/repeat "." (- i min))
					 (string/repeat "#" size))]]
	     (if (= 1 (length sizes))
	       ## if last group, match candidate against conditions
	       (let [full-cand (string next-cand
				       (string/repeat
					 "." (- len (length next-cand))))]
		 (if (match-candidate full-cand spring-conds)
		   (do
		     (print full-cand)
		     1)
		   0))
	       ## else, recurse with the next group size
	       (rec (slice sizes 1)
		    next-cand
		    (+ i size 1)))))))
      group-sizes "" 0))
  
  #
  )

(comment
  ## Failed attempt:

  (seq [[spring-conds group-sizes] :in (peg/match spring-records-peg
						  ex2)]
    (print "=================================")
    (printf "%q : %q" spring-conds group-sizes)
    ((fn rec [spring-conds group-sizes cursor]
       (let [ ## limit → size of groups + gaps between them
	     limit (+ (sum group-sizes) (dec (length group-sizes)))
	     gsize (first group-sizes)]
	 (var matches @[])
	 (var first-hash nil)
	 ## collect matches
	 (for i cursor (inc (- (length spring-conds) limit)) # ! check
	   (let [group (string/slice spring-conds i (+ i gsize))
		 observe (let [idx-group (map tuple (range i (+ i gsize)) group)]
			   (fn [char]
			     (some |(when (= ($ 1) char) ($ 0)) idx-group)))
		 next  (get spring-conds (+ i gsize))]
	     (when (not= next hash)
	       (let [obs-hash  (observe hash)
		     obs-qmark (observe qmark)
		     obs-dot   (observe dot)]
		 (when obs-hash
		   (if first-hash
		     ## first seen hash/fact must be part of any match
		     (when (not= first-hash obs-hash) (break))
		     ## all guessed matches must give way to an observed facts
		     (do (set first-hash obs-hash)
		       # (set matches @[])
		       )))
		 (cond
		   ## first seen hash/fact must be part of any match
		   (and first-hash (not obs-hash)) (break)
		   ## match must be continuous
		   obs-dot nil
		   obs-qmark (array/push matches [group i])
		   :else (do (array/push matches [group i])
			   (break)))))))
	 (pp matches)
	 (cond
	   (empty? matches) (do (print "FAIL") 0)
	   (= 1 (length group-sizes)) (do (print (string "=> " (length matches)))
					(length matches))
	   :else (sum (seq [[group i] :in matches]
			(rec spring-conds
			     (slice group-sizes 1)
			     ## +1 to skip delimiter, which is assumed as '.'
			     (+ i (length group) 1)))))))
      spring-conds group-sizes 0))

  #
  )
