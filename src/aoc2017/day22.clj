(ns aoc2017.day22
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)


;;; Part 1

;; Diagnostics indicate that the local grid computing cluster has been
;; contaminated with the Sporifica Virus. The grid computing cluster is a
;; seemingly-infinite two-dimensional grid of compute nodes. Each node is
;; either clean or infected by the virus.

;; To prevent overloading the nodes (which would render them useless to the
;; virus) or detection by system administrators, exactly one virus carrier
;; moves through the network, infecting or cleaning nodes as it moves. The
;; virus carrier is always located on a single node in the network (the
;; current node) and keeps track of the direction it is facing.

;; To avoid detection, the virus carrier works in bursts; in each burst, it
;; wakes up, does some work, and goes back to sleep. The following steps are
;; all executed in order one time each burst:

;; - If the current node is infected, it turns to its right. Otherwise, it
;;   turns to its left. (Turning is done in-place; the current node does not
;;   change.)

;; - If the current node is clean, it becomes infected. Otherwise, it becomes
;;   cleaned. (This is done after the node is considered for the purposes of
;;   changing direction.)

;; - The virus carrier moves forward one node in the direction it is facing.

;; Diagnostics have also provided a map of the node infection status (your
;; puzzle input). Clean nodes are shown as .; infected nodes are shown as
;; #. This map only shows the center of the grid; there are many more nodes
;; beyond those shown, but none of them are currently infected.

;; The virus carrier begins in the middle of the map facing up.

;; For example, suppose you are given a map like this:

;; ..#
;; #..
;; ...

;; Then, the middle of the infinite grid looks like this, with the virus
;; carrier's position marked with [ ]:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . # . . .
;; . . . #[.]. . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; The virus carrier is on a clean node, so it turns left, infects the node,
;; and moves left:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . # . . .
;; . . .[#]# . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; The virus carrier is on an infected node, so it turns right, cleans the
;; node, and moves up:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . .[.]. # . . .
;; . . . . # . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; Four times in a row, the virus carrier finds a clean, infects it, turns
;; left, and moves forward, ending in the same place and still facing up:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . #[#]. # . . .
;; . . # # # . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; Now on the same node as before, it sees an infection, which causes it to
;; turn right, clean the node, and move forward:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . # .[.]# . . .
;; . . # # # . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; After the above actions, a total of 7 bursts of activity had taken
;; place. Of them, 5 bursts of activity caused an infection.

;; After a total of 70, the grid looks like this, with the virus carrier
;; facing up:

;; . . . . . # # . .
;; . . . . # . . # .
;; . . . # . . . . #
;; . . # . #[.]. . #
;; . . # . # . . # .
;; . . . . . # # . .
;; . . . . . . . . .
;; . . . . . . . . .

;; By this time, 41 bursts of activity caused an infection (though most of
;; those nodes have since been cleaned).

;; After a total of 10000 bursts of activity, 5587 bursts will have caused an
;; infection.

;; Given your actual map, after 10000 bursts of activity, how many bursts
;; cause a node to become infected? (Do not count nodes that begin infected.)

(defn parse-input
  [input]
  (let [grid (->> input
                  str/split-lines
                  (map vec)
                  vec)
        n (count grid)
        m (quot n 2)]
    (->> (for [row (range n)
               col (range n)]
           [[(- col m) (- m row)]
            (get-in grid [row col])])
         (into {}))))

(def dirs [:up :right :down :left])

(def dir->delta
  {:up [0 1]
   :right [1 0]
   :down [0 -1]
   :left [-1 0]})

(defn change-dir
  [dir turn]
  (let [idx (.indexOf dirs dir)]
    (case turn
      :left (nth dirs (mod (dec idx) 4))
      :right (nth dirs (mod (inc idx) 4))
      :none dir
      :reverse (nth dirs (mod (+ 2 idx) 4)))))

(defn decide-turn
  [state]
  (if (= \# state)
    :right
    :left))

(defn change
  [grid-map pos]
  (let [c (if (= \# (get grid-map pos))
            \.
            \#)]
    (assoc grid-map pos c)))

(defn num-infecting-bursts
  [num-bursts grid-map]
  (loop [i num-bursts
         grid-map grid-map
         pos [0 0]
         dir :up
         k 0]
    (if (zero? i)
      k
      (let [c (get grid-map pos)
            new-dir (change-dir dir (decide-turn c))
            new-grid-map (change grid-map pos)
            new-pos (mapv + pos (dir->delta new-dir))]
        (recur (dec i)
               new-grid-map
               new-pos
               new-dir
               (if-not (= \# c) (inc k) k))))))


;;; Part 2

;; As you go to remove the virus from the infected nodes, it evolves to resist
;; your attempt.

;; Now, before it infects a clean node, it will weaken it to disable your
;; defenses. If it encounters an infected node, it will instead flag the node
;; to be cleaned in the future. So:

;; - Clean nodes become weakened.

;; - Weakened nodes become infected.

;; - Infected nodes become flagged.

;; - Flagged nodes become clean.

;; Every node is always in exactly one of the above states.

;; The virus carrier still functions in a similar way, but now uses the
;; following logic during its bursts of action:

;; - Decide which way to turn based on the current node:

;;   - If it is clean, it turns left.

;;   - If it is weakened, it does not turn, and will continue moving in the
;;     same direction.

;;   - If it is infected, it turns right.

;;   - If it is flagged, it reverses direction, and will go back the way it
;;     came.

;; - Modify the state of the current node, as described above.

;; - The virus carrier moves forward one node in the direction it is facing.

;; Start with the same map (still using . for clean and # for infected) and
;; still with the virus carrier starting in the middle and facing up.

;; Using the same initial state as the previous example, and drawing weakened
;; as W and flagged as F, the middle of the infinite grid looks like this,
;; with the virus carrier's position again marked with [ ]:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . # . . .
;; . . . #[.]. . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; This is the same as before, since no initial nodes are weakened or
;; flagged. The virus carrier is on a clean node, so it still turns left,
;; instead weakens the node, and moves left:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . # . . .
;; . . .[#]W . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; The virus carrier is on an infected node, so it still turns right, instead
;; flags the node, and moves up:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . .[.]. # . . .
;; . . . F W . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; This process repeats three more times, ending on the previously-flagged
;; node and facing right:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . W W . # . . .
;; . . W[F]W . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; Finding a flagged node, it reverses direction and cleans the node:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . W W . # . . .
;; . .[W]. W . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; The weakened node becomes infected, and it continues in the same direction:

;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . W W . # . . .
;; .[.]# . W . . . .
;; . . . . . . . . .
;; . . . . . . . . .
;; . . . . . . . . .

;; Of the first 100 bursts, 26 will result in infection. Unfortunately,
;; another feature of this evolved virus is speed; of the first 10000000
;; bursts, 2511944 will result in infection.

;; Given your actual map, after 10000000 bursts of activity, how many bursts
;; cause a node to become infected? (Do not count nodes that begin infected.)

(defn decide-turn-evolved
  [state]
  (case (or state \.)
    \. :left
    \W :none
    \# :right
    \F :reverse))

(defn change-evolved
  [grid-map pos]
  (let [new-state (case (get grid-map pos \.)
                    \. \W
                    \W \#
                    \# \F
                    \F \.)]
    (assoc grid-map pos new-state)))

(defn num-infecting-bursts-evolved
  [num-bursts grid-map]
  (loop [i num-bursts
         grid-map grid-map
         pos [0 0]
         dir :up
         k 0]
    (if (zero? i)
      k
      (let [c (get grid-map pos)
            new-dir (change-dir dir (decide-turn-evolved c))
            new-grid-map (change-evolved grid-map pos)
            new-pos (mapv + pos (dir->delta new-dir))]
        (recur (dec i)
               new-grid-map
               new-pos
               new-dir
               (if (= \W c) (inc k) k))))))
