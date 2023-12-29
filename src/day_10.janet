(use ./utils)

(def input (slurp "../input/day_10.txt"))

(def char->tile
  {"S" :start
   "." :ground
   "|" [:N :S]
   "-" [:E :W]
   "L" [:N :E]
   "J" [:N :W]
   "7" [:S :W]
   "F" [:S :E]})

(def dir->coords
  {:N [ 0 -1]
   :S [ 0  1]
   :E [ 1  0]
   :W [-1  0]})

(def coords->dir
  (invert dir->coords))

(defn tile-from-coords [[x y] [ax ay] [bx by]]
  (let [da [(- ax x) (- ay y)]
	db [(- bx x) (- by y)]]
    (slice (sort-by |(case $ :N 0 :S 1 :E 2 :W 3)
		    @[(coords->dir da)
		      (coords->dir db)]))))

(def pipes-peg
  ~{:char (group (* (column) (line)
		    (/ (<- (set "|-LJ7FS.")) ,char->tile)))
    :line (group (* (some :char) "\n"))
    :main (some :line)})

(defn make-pipes [rows]
  (var start-xy nil)
  (let [shift (fn [[x y] [dx dy]] [(+ x dx) (+ y dy)])
	col-to-pipe
	(fn [[x y tile]]
	  (let [m (match tile
		    [da db] (let [a (shift [x y] (dir->coords da))
				  b (shift [x y] (dir->coords db))]
			      {a b b a})
		    :start (do (set start-xy [x y]) ## side-effect!
			     tile)
		    nil)] ## other tiles will vanish in pipes map
	    [[x y] m]))
	pipes-arr   (mapcat |(map col-to-pipe $) rows)
	pipes       (from-pairs pipes-arr)
	## find pipes that join the start pipe
	start-joins (->> pipes-arr
			 (filter (fn [[k v]] (when (struct? v) (v start-xy))))
			 (map first))]
    ## reconstruct start pipe by connecting the joining pipes
    ## also reconstruct the tile to get start directions for part 2
    (let [start-map  {(start-joins 0) (start-joins 1)
		      (start-joins 1) (start-joins 0)}
	  start-tile (tile-from-coords start-xy
				       (start-joins 0) (start-joins 1))]
      [(put pipes start-xy start-map)
       [start-xy start-tile]])))

(defn traverse-loop [pipes start-xy]
  (var step 0)
  (var curr-pipe start-xy)
  (var next-pipe ((pipes start-xy) (last (keys (pipes start-xy)))))
  (var loop-trail @[curr-pipe])
  (while (not= next-pipe start-xy)
    (array/push loop-trail next-pipe)
    (let [from-pipe curr-pipe]
      (set curr-pipe next-pipe)
      (set next-pipe ((pipes next-pipe) from-pipe)))
    (if (> step 99999)
      (error "No loop, something must be wrong.")
      (+= step 1)))
  loop-trail)


(defn solve1 [input]
  (let [rows (peg/match pipes-peg input)
	[pipes [start-xy _]] (make-pipes rows)
	loop-trail (traverse-loop pipes start-xy)]
    (/ (length loop-trail) 2)))


(defn count-tiles-inside-loop [rows start-tile loop-trail]
  (var count 0)
  (each row rows
    (var inside? false)
    (var bound-start nil)
    (each [x y tile] row
      (let [tile (if (= :start tile) start-tile tile)]
	## The arrangement of loop tiles in a row indicate where it
	## is being crossed to the inside and back out again.
	## This can be used as a switch to count the tiles inside of it.
	(if (and (tuple? tile) (has-value? loop-trail [x y]))
	  (match [bound-start tile]
	    ## |
	    [nil [:N :S]] (set-not inside?)
	    ## ┌...
	    [nil [:S :E]] (set bound-start [:S :E])
	    ## └...
	    [nil [:N :E]] (set bound-start [:N :E])
	    ## ┌─┘
	    [[:S :E] [:N :W]] (do (set-not inside?) (set bound-start nil))
	    ## └─┐
	    [[:N :E] [:S :W]] (do (set-not inside?) (set bound-start nil))
	    ## ┌─┐
	    [[:S :E] [:S :W]] (set bound-start nil)
	    ## └─┘
	    [[:N :E] [:N :W]] (set bound-start nil)
	    ## ...─...
	    [[] [:E :W]] nil
	    ## anything else (unreachable?)
	    (set bound-start nil))
	  (when inside?
	    (+= count 1))))))
  count)

(defn solve2 [input]
  (let [rows (peg/match pipes-peg input)
	[pipes [start-xy start-tile]] (make-pipes rows)
	loop-trail (traverse-loop pipes start-xy)]
    (count-tiles-inside-loop rows start-tile loop-trail)))


(defn main [& args]
  (aoc-print 10 1 (solve1 input) 6864)
  (aoc-print 10 2 (solve2 input) 349))


(comment

  (def sample-1a
    (string
      `
-L|F7
7S-7|
L|7||
-L-J|
L|-JF` "\n"))

  (def sample-1b
    (string
      `
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ` "\n"))


  (def sample-2a
    (string
      `
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........` "\n"))
  (def sample-2a-marked
    (string
      `
...........
.S-------7.
.|F-----7|.
.||OOOOO||.
.||OOOOO||.
.|L-7OF-J|.
.|II|O|II|.
.L--JOL--J.
.....O.....` "\n"))

  (def sample-2b
    (string
      `
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...` "\n"))
  (def sample-2b-marked
    (string
      `
OF----7F7F7F7F-7OOOO
O|F--7||||||||FJOOOO
O||OFJ||||||||L7OOOO
FJL7L7LJLJ||LJIL-7OO
L--JOL7IIILJS7F-7L7O
OOOOF-JIIF7FJ|L7L7L7
OOOOL7IF7||L7|IL7L7|
OOOOO|FJLJ|FJ|F7|OLJ
OOOOFJL-7O||O||||OOO
OOOOL---JOLJOLJLJOOO` "\n"))

  (def sample-2c
    (string
      `
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L` "\n"))
  (def sample-2c-marked
    (string
      `
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L` "\n"))

  (defn prettify-pipes
    "Creates more readable pipe-maps using unicode characters."
    [input]
    (->> input
	 (peg/replace-all "|" "│")
	 (peg/replace-all "-" "─")
	 (peg/replace-all "L" "└")
	 (peg/replace-all "J" "┘")
	 (peg/replace-all "7" "┐")
	 (peg/replace-all "F" "┌")
	 (peg/replace-all "I" "▓")
	 (peg/replace-all "O" "░")))

  (print "\n" (prettify-pipes sample-2a-marked))
  (print "\n" (prettify-pipes sample-2b-marked))
  (print "\n" (prettify-pipes sample-2c-marked))

  ## not really helping much, but nice to look at
  (spit "../output/day_10_pipes.txt" (prettify-pipes input))
 
  )

(comment

  (let [rows (peg/match pipes-peg sample-1a)
	[pipes [start-xy _]] (make-pipes rows)
	loop (traverse-loop pipes start-xy)]
    (/ (length loop) 2))


  ## tested with examples 2a, 2b and 2c
  (let [rows (peg/match pipes-peg sample-2c)
	[pipes [start-xy start-tile]] (make-pipes rows)
	loop-trail (traverse-loop pipes start-xy)
	inner-tiles @[]]
    (each row rows
      (var inside? false)
      (var bound-start nil)
      (each [x y tile] row
	(let [tile (if (= :start tile) start-tile tile)]
	  (if (and (tuple? tile) (has-value? loop-trail [x y]))
	    (match [bound-start tile]
	      ## |
	      [nil [:N :S]] (set-not inside?)
	      ## ┌...
	      [nil [:S :E]] (set bound-start [:S :E])
	      ## └...
	      [nil [:N :E]] (set bound-start [:N :E])
	      ## ┌─┘
	      [[:S :E] [:N :W]] (do (set-not inside?) (set bound-start nil))
	      ## └─┐
	      [[:N :E] [:S :W]] (do (set-not inside?) (set bound-start nil))
	      ## ┌─┐
	      [[:S :E] [:S :W]] (set bound-start nil)
	      ## └─┘
	      [[:N :E] [:N :W]] (set bound-start nil)
	      ## ...─...
	      [[] [:E :W]] nil
	      ## anything else
	      (set bound-start nil))
	    (when inside?
	      (array/push inner-tiles [x y tile]))))))
    inner-tiles)

  )

(comment

  ##       [3 2]
  ## [2 3] [3 3] [4 3]
  ##       [3 4]

  ((fn [[x y] [ax ay] [bx by]]
     (let [da [(- ax x) (- ay y)]
	   db [(- bx x) (- by y)]]
       (slice (sort-by |(case $ :N 0 :S 1 :E 2 :W 3)
		       @[(coords->dir da)
			 (coords->dir db)]))))
    [3 3] [4 3] [2 3])

  )
