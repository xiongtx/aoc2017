(ns aoc2017.day11
  (:require [clojure.string :as str]))


;;; Part 1

;; Crossing the bridge, you've barely reached the other side of the stream
;; when a program comes up to you, clearly in distress. "It's my child
;; process," she says, "he's gotten lost in an infinite grid!"

;; Fortunately for her, you have plenty of experience with infinite grids.

;; Unfortunately for you, it's a hex grid.

;; The hexagons ("hexes") in this grid are aligned such that adjacent hexes
;; can be found to the north, northeast, southeast, south, southwest, and
;; northwest:

;;   \ n  /
;; nw +--+ ne
;;   /    \
;; -+      +-
;;   \    /
;; sw +--+ se
;;   / s  \

;; You have the path the child process took. Starting where he started, you
;; need to determine the fewest number of steps required to reach
;; him. (A "step" means to move from the hex you are in to any adjacent hex.)

;; For example:

;; - ne,ne,ne is 3 steps away.

;; - ne,ne,sw,sw is 0 steps away (back where you started).

;; - ne,ne,s,s is 2 steps away (se,se).

;; - se,sw,se,sw,sw is 3 steps away (s,s,sw).

(defn parse-input
  [input]
  (-> input
      str/trim-newline
      (str/split #",")))

;; See: http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
(def dir->delta
  "Coordinate change for direction."
  {"n" [-1 1 0]
   "ne" [0 1 -1]
   "se" [1 0 -1]
   "s" [1 -1 0]
   "sw" [0 -1 1]
   "nw" [-1 0 1]})

(defn move
  [coords dir]
  (mapv + coords (dir->delta dir)))

(defn distance
  ([c1]
   (distance c1 [0 0 0]))
  ([c1 c2]
   (->> (map - c1 c2)
        (map #(Math/abs %))
        (apply max))))

(defn final-coords
  [dirs]
  (reduce move [0 0 0] dirs))

(defn final-distance
  [dirs]
  (distance (final-coords dirs)))


;;; Part 2

;; How many steps away is the furthest he ever got from his starting position?

(defn further?
  [c1 c2]
  (> (distance c1) (distance c2)))

(defn furthest-coords
  [dirs]
  (-> (reduce (fn [[coords furthest] d]
                (let [new-coords (move coords d)]
                  [new-coords
                   (if (further? new-coords furthest)
                     new-coords
                     furthest)]))
              [[0 0 0] [0 0 0]]
              dirs)
      second))

(defn furthest-distance
  [dirs]
  (distance (furthest-coords dirs)))
