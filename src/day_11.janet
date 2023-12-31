(use ./utils)

(def input (slurp "../input/day_11.txt"))

(def galaxies-peg
  ~{:char (group (* (column) (line)
		    (set "#")))
    :line (* (some (+ :char ".")) "\n")
    :main (some :line)})

(defn expand-universe [factor old-galaxies]
  (let [galcols (sort (distinct (map first old-galaxies)))
	galrows (sort (distinct (map last old-galaxies)))
	max-galcol (last galcols)
	max-galrow (last galrows)
	galaxies (array/slice old-galaxies)]
    (for i 1 (inc (max max-galcol max-galrow))
      (let [expand-x? (and (<= i max-galcol) (not (has-value? galcols i)))
	    expand-y? (and (<= i max-galrow) (not (has-value? galrows i)))]
	(when (or expand-x? expand-y?)
	  (eachp [k [x y]] old-galaxies
	    (let [inc-x (if (and expand-x? (> x i)) (dec factor) 0)
		  inc-y (if (and expand-y? (> y i)) (dec factor) 0)]
	      (when (or (> inc-x 0) (> inc-y 0))
		(update galaxies k |[(+ ($ 0) inc-x)
				     (+ ($ 1) inc-y)])))))))
    galaxies))

(defn shortest-paths-between [galaxies]
  (->> (unique-pairs galaxies true)
       (map (fn [[[x1 y1] [x2 y2]]]
	      (+ (math/abs (- x2 x1))
		 (math/abs (- y2 y1)))))))


(defn solve1 [input]
  (->> (peg/match galaxies-peg input)
       (expand-universe 2)
       shortest-paths-between
       sum))


(defn solve2 [input]
  (->> (peg/match galaxies-peg input)
       (expand-universe 1000000)
       shortest-paths-between
       sum))


(defn main [& args]
  (aoc-print 11 1 (solve1 input) 10231178)
  (aoc-print 11 2 (solve2 input) 622120986954))


(comment

  (def ex1 (aoc-example `
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
`))

  (defn num-of-pairs [n]
    (/ (* n (dec n)) 2))

  (let [galaxies (peg/match galaxies-peg ex1)]
    (let [galaxies-expanded (expand-universe 2 galaxies)
	  shortest-paths (shortest-paths-between galaxies-expanded)]
      (sum shortest-paths)))
  
  ## 1: 4,1 -> 5,1
  ## 2: 8,2 -> 10,2
  ## 3: 1,3 -> 1,3
  ## 4: 7,5 -> 9,6
  ## 5: 2,6 -> 2,7
  ## 6: 10,7 -> 13,8
  ## 7: 8,9 -> 10,11
  ## 8: 1,10 -> 1,12
  ## 9: 5,10 -> 6,12

  (let [galaxies (peg/match galaxies-peg ex1)]
    (let [galaxies-expanded (expand-universe 10 galaxies)
	  shortest-paths (shortest-paths-between galaxies-expanded)]
      (sum shortest-paths)))

  )

(comment
  `
 ...#......
 .......#..
 #.........
 ..........
 ......#...
 .#........
 .........#
 ..........
 .......#..
 #...#.....
`
  `
 ....1........
 .........2...
 3............
 .............
 .............
 ........4....
 .5...........
 ............6
 .............
 .............
 .........7...
 8....9.......
`
  )

(comment
  ## shortest paths in grids are actually just the sum of the row and
  ## column distances between two points

  ## why? because there has to be at least this number of steps to reach
  ## the other point and there cannot be more steps since then one
  ## would have to go back in the opposite direction
  
  `
....1........
....#....2...
3...##.......
.....#.......
.....##......
......#.4....
.5....##.....
.##....#....6
..##...##....
...##...#....
....##..#7...
8....9.......
`
  )
