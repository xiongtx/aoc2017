(ns aoc2017.day19
  (:require [clojure.string :as str]))


;;; Part 1

;; Somehow, a network packet got lost and ended up here. It's trying to follow
;; a routing diagram (your puzzle input), but it's confused about where to go.

;; Its starting point is just off the top of the diagram. Lines (drawn with |,
;; -, and +) show the path it needs to take, starting by going down onto the
;; only line connected to the top of the diagram. It needs to follow this path
;; until it reaches the end (located somewhere within the diagram) and stop
;; there.

;; Sometimes, the lines cross over each other; in these cases, it needs to
;; continue going the same direction, and only turn left or right when there's
;; no other option. In addition, someone has left letters on the line; these
;; also don't change its direction, but it can use them to keep track of where
;; it's been. For example:

;;     |
;;     |  +--+
;;     A  |  C
;; F---|----E|--+
;;     |  |  |  D
;;     +B-+  +--+

;; Given this diagram, the packet needs to take the following path:

;; - Starting at the only line touching the top of the diagram, it must go
;;   down, pass through A, and continue onward to the first +.

;; - Travel right, up, and right, passing through B in the process.

;; - Continue down (collecting C), right, and up (collecting D).

;; - Finally, go all the way left through E and stopping at F.

;; Following the path to the end, the letters it sees on its path are ABCDEF.

;; The little packet looks up at you, hoping you can help it find the
;; way. What letters will it see (in the order it would see them) if it
;; follows the path? (The routing diagram is very wide; make sure you view it
;; without line wrapping.)

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (mapv vec)))

(def dir->delta
  {:up [-1 0]
   :right [0 1]
   :down [1 0]
   :left [0 -1]})

(def dir-pair
  {:up :down
   :right :left
   :down :up
   :left :right})

(defn move-delta
  [pos delta]
  (mapv + pos delta))

(defn out-of-bounds?
  [diagram [x y]]
  (let [x-lim (count diagram)
        y-lim (count (first diagram))]
    (not (and (<= 0 x (dec x-lim)) (<= 0 y (dec y-lim))))))

(defn movable?
  "Can move into position pos?"
  [diagram pos]
  (not (or (out-of-bounds? diagram pos)
         (= \space (get-in diagram pos)))))

(defn change-dir
  [diagram pos dir]
  (let [pos-same-dir (move-delta pos (dir->delta dir))]
    (if (movable? diagram pos-same-dir)
      dir
      (let [[[dir1 delta1] [dir2 delta2]] (-> dir->delta
                                              (dissoc dir (dir-pair dir))
                                              vec)]
        (if (movable? diagram (move-delta pos delta1))
          dir1
          dir2)))))

(defn letter?
  [c]
  (<= (int \A) (int c) (int \Z)))

(defn travel
  [diagram]
  (loop [pos [0 (.indexOf (first diagram) \|)]
         dir :down
         letters []]
    (let [new-pos (move-delta pos (dir->delta dir))
          new-c (get-in diagram new-pos)]
      (if-not (movable? diagram new-pos)
        (str/join letters)
        (cond
          (= \+ new-c) (recur new-pos
                              (change-dir diagram new-pos dir)
                              letters)
          (letter? new-c) (recur new-pos dir (conj letters new-c))
          :else (recur new-pos dir letters))))))


;;; Part 2

;; The packet is curious how many steps it needs to go.

;; For example, using the same routing diagram from the example above...

 ;;     |
 ;;     |  +--+
 ;;     A  |  C
 ;; F---|--|-E---+
 ;;     |  |  |  D
 ;;     +B-+  +--+

;; ...the packet would go:

;; - 6 steps down (including the first line at the top of the diagram).

;; - 3 steps right.

;; - 4 steps up.

;; - 3 steps right.

;; - 4 steps down.

;; - 3 steps right.

;; - 2 steps up.

;; - 13 steps left (including the F it stops on).

;; This would result in a total of 38 steps.

;; How many steps does the packet need to go?

(defn num-steps
  [diagram]
  (loop [pos [0 (.indexOf (first diagram) \|)]
         dir :down
         steps 1]
    (let [new-pos (move-delta pos (dir->delta dir))]
      (if-not (movable? diagram new-pos)
        steps
        (recur new-pos
               (if (= \+ (get-in diagram new-pos))
                 (change-dir diagram new-pos dir)
                 dir)
               (inc steps))))))
