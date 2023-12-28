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

(def pipes-peg
  ~{:char (group (* (column) (line)
		    (/ (<- (set "|-LJ7FS")) ,char->tile)))
    :line (group (* (some (+ :char ".")) "\n"))
    :main (some :line)})


(defn make-pipes [rows]
  (var start nil)
  (let [shift (fn [[x y] [dx dy]] [(+ x dx) (+ y dy)])
	col-to-pipe
	(fn [[x y tile]]
	  (let [m (if (tuple? tile)
		    ## assumes only pipes except one single 'S'
		    (let [a (shift [x y] (dir->coords (tile 0)))
			  b (shift [x y] (dir->coords (tile 1)))]
		      {a b b a})
		    (do (set start [x y]) ## side-effect!
		      tile))]
	    [[x y] m]))
	pipes-arr   (mapcat |(map col-to-pipe $) rows)
	pipes       (from-pairs pipes-arr)
	## find pipes that join the :start pipe
	start-joins (->> pipes-arr
			 (filter (fn [[k v]] (when (struct? v) (v start))))
			 (map first))]
    ## reconstruct :start pipe by connecting the joining pipes
    [(put pipes start {(start-joins 0) (start-joins 1)
		       (start-joins 1) (start-joins 0)})
     start]))

(defn traverse-loop [pipes start]
  (var step 0)
  (var curr-pipe start)
  (var next-pipe ((pipes start) (last (keys (pipes start)))))
  (var trail @[curr-pipe])
  (while (not= next-pipe start)
    (array/push trail next-pipe)
    (let [from-pipe curr-pipe]
      (set curr-pipe next-pipe)
      (set next-pipe ((pipes next-pipe) from-pipe)))
    (if (> step 99999)
      (error "No loop, something must be wrong.")
      (+= step 1)))
  trail)

(defn solve1 [input]
  (let [rows (peg/match pipes-peg input)
	[pipes start] (make-pipes rows)
	loop (traverse-loop pipes start)]
    (/ (length loop) 2)))


(defn solve2 [input]
  nil)


(defn main [& args]
  (aoc-print 10 1 (solve1 input) 6864)
  (aoc-print 10 2 (solve2 input) "???"))


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


  (let [rows (peg/match pipes-peg sample-1a)
	[pipes start] (make-pipes rows)
	loop (traverse-loop pipes start)]
    (/ (length loop) 2))


  )
