(ns aoc2017.day3)


;;; Part 1

;; You come across an experimental new kind of memory stored on an infinite
;; two-dimensional grid.

;; Each square on the grid is allocated in a spiral pattern starting at a
;; location marked 1 and then counting up while spiraling outward. For
;; example, the first few squares are allocated like this:

;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6   1   2  11
;; 20   7   8   9  10
;; 21  22  23---> ...

;; While this is very space-efficient (no squares are skipped), requested data
;; must be carried back to square 1 (the location of the only access port for
;; this memory system) by programs that can only move up, down, left, or
;; right. They always take the shortest path: the Manhattan Distance between
;; the location of the data and square 1.

;; For example:

;; - Data from square 1 is carried 0 steps, since it's at the access port.

;; - Data from square 12 is carried 3 steps, such as: down, left, left.

;; - Data from square 23 is carried only 2 steps: up twice.

;; - Data from square 1024 must be carried 31 steps.

;; How many steps are required to carry the data from the square identified in
;; your puzzle input all the way to the access port?

(def dirs
  "Directions in order of spiral."
  [:right :up :left :down])

(def dir-steps
  "Map of directions and coordinate steps."
  {:right [1 0]
   :up [0 1]
   :left [-1 0]
   :down [0 -1]})

(defn add-coords
  [c1 c2]
  (mapv + c1 c2))

(defn next-dir
  [dir]
  (nth dirs (mod (inc (.indexOf dirs dir)) (count dirs))))

(defn coordinates
  "Cartesian coordinates for n."
  [n]
  {:pre [(integer? n) (pos? n)]}
  (loop [m 1
         xy [0 0]
         dir :right
         num-steps 1
         next-steps (->> (interleave (range) (range))
                         (drop 3))]
    (if (= m n)
      xy
      (let [num-steps (dec num-steps)]
        (recur (inc m)
               (add-coords xy (get dir-steps dir))
               (if (zero? num-steps) (next-dir dir) dir)
               (if (zero? num-steps) (first next-steps) num-steps)
               (if (zero? num-steps) (rest next-steps) next-steps))))))

(defn manhattan-distance
  "Manhattan distance of coordinate (x, y)."
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn distance
  "Distance of n to 1."
  [n]
  {:pre [(integer? n) (pos? n)]}
  (manhattan-distance (coordinates n)))


;;; Part 2

;; As a stress test on the system, the programs here clear the grid and then
;; store the value 1 in square 1. Then, in the same allocation order as shown
;; above, they store the sum of the values in all adjacent squares, including
;; diagonals.

;; So, the first few squares' values are chosen as follows:

;; - Square 1 starts with the value 1.

;; - Square 2 has only one adjacent filled square (with value 1), so it also
;;   stores 1.

;; - Square 3 has both of the above squares as neighbors and stores the sum of
;;   their values, 2.

;; - Square 4 has all three of the aforementioned squares as neighbors and
;;   stores the sum of their values, 4.

;; - Square 5 only has the first and fourth squares as neighbors, so it gets
;;   the value 5.

;; Once a square is written, its value does not change. Therefore, the first
;; few squares would receive the following values:

;; 147  142  133  122   59
;; 304    5    4    2   57
;; 330   10    1    1   54
;; 351   11   23   25   26
;; 362  747  806--->   ...

;; What is the first value written that is larger than your puzzle input?

;; From: https://stackoverflow.com/a/9971465/864684
(defn coordinates->index
  "Convert Cartesian coordinates to index."
  [[x y]]
  (if (>= (* y y) (* x x))
    (let [idx (- (* 4 y y) y x)]
      (if (< y x)
        (- idx (* 2 (- y x)))
        idx))
    (let [idx (- (* 4 x x) y x)]
      (if (< y x)
        (+ idx (* 2 (- y x)))
        idx))))

(defn index->coordinates
  "Convert index to Cartesian coordinates."
  [idx]
  (coordinates (inc idx)))

(defn neighbors
  "Return all neighbors of coord (x, y)."
  [[x y]]
  (for [i [(dec x) x (inc x)]
        j [(dec y) y (inc y)]
        :when (not (and (= i x) (= j y)))]
    [i j]))

(defn sum-larger-than
  "Return the first spiral sum larger than n."
  [k]
  (loop [spiral [1]]
    (let [v (peek spiral)]
      (if (> v k)
        v
        (let [nv (->> (count spiral)
                      index->coordinates
                      neighbors
                      (map coordinates->index)
                      (map #(nth spiral % 0))
                      (apply +))]
          (recur (conj spiral nv)))))))
