(ns aoc2017.day21
  (:require [clojure.string :as str]))


;;; Part 1

;; You find a program trying to generate some art. It uses a strange process
;; that involves repeatedly enhancing the detail of an image through a set of
;; rules.

;; The image consists of a two-dimensional square grid of pixels that are
;; either on (#) or off (.). The program always begins with this pattern:

;; .#.
;; ..#
;; ###

;; Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to
;; have a size of 3.

;; Then, the program repeats the following process:

;; If the size is evenly divisible by 2, break the pixels up into 2x2 squares,
;; and convert each 2x2 square into a 3x3 square by following the
;; corresponding enhancement rule.

;; - Otherwise, the size is evenly divisible by 3; break the pixels up into
;;   3x3 squares, and convert each 3x3 square into a 4x4 square by following
;;   the corresponding enhancement rule.

;; - Because each square of pixels is replaced by a larger one, the image
;;   gains pixels and so its size increases.

;; The artist's book of enhancement rules is nearby (your puzzle input);
;; however, it seems to be missing rules. The artist explains that sometimes,
;; one must rotate or flip the input pattern to find a match. (Never rotate or
;; flip the output pattern, though.) Each pattern is written concisely: rows
;; are listed as single units, ordered top-down, and separated by slashes. For
;; example, the following rules correspond to the adjacent patterns:

;; ../.#  =  ..
;;           .#

;;                 .#.
;; .#./..#/###  =  ..#
;;                 ###

;;                         #..#
;; #..#/..../#..#/.##.  =  ....
;;                         #..#
;;                         .##.

;; When searching for a rule to use, rotate and flip the pattern as
;; necessary. For example, all of the following patterns match the same rule:

;; .#.   .#.   #..   ###
;; ..#   #..   #.#   ..#
;; ###   ###   ##.   .#.

;; Suppose the book contained the following two rules:

;; ../.# => ##./#../...
;; .#./..#/### => #..#/..../..../#..#

;; As before, the program begins with this pattern:

;; .#.
;; ..#
;; ###

;; The size of the grid (3) is not divisible by 2, but it is divisible by
;; 3. It divides evenly into a single square; the square matches the second
;; rule, which produces:

;; #..#
;; ....
;; ....
;; #..#

;; The size of this enhanced grid (4) is evenly divisible by 2, so that rule
;; is used. It divides evenly into four squares:

;; #.|.#
;; ..|..
;; --+--
;; ..|..
;; #.|.#

;; Each of these squares matches the same rule (../.# => ##./#../...), three
;; of which require some flipping and rotation to line up with the rule. The
;; output for the rule is the same in all four cases:

;; ##.|##.
;; #..|#..
;; ...|...
;; ---+---
;; ##.|##.
;; #..|#..
;; ...|...

;; Finally, the squares are joined into a new grid:

;; ##.##.
;; #..#..
;; ......
;; ##.##.
;; #..#..
;; ......

;; Thus, after 2 iterations, the grid contains 12 pixels that are on.

;; How many pixels stay on after 5 iterations?

(def start [[\. \# \.] [\. \. \#] [\# \# \#]])

(defn parse-flat
  [flat]
  (->> (str/split flat #"/")
       (mapv vec)))

(defn parse-line
  [line]
  (->> (str/split line #" => ")
       (mapv parse-flat)))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into {})))

(defn flipud
  [mat]
  (reverse mat))

(defn fliplr
  [mat]
  (map reverse mat))

(defn rot90
  ([mat]
   (rot90 mat 1))
  ([mat k]
   (let [n (count mat)]
     (case (mod k 4)
       0 mat
       ;; Avoid a reverse
       1 (for [i (range (dec n) (dec 0)-1)]
           (map #(nth % i) mat))
       2 (-> mat rot90 rot90)
       ;; Naive (-> mat rot90 rot90 rot90) is unnecessary calculation on every
       ;; inner loop
       3 (for [i (range n)]
           (map #(nth % (- n i 1)) mat))))))

(defn transforms
  "Set of transforms of mat via rotation / flip."
  [mat]
  (let [rts (for [i (range (count mat))]
              (rot90 mat i))]
    (into #{} (concat rts
                      (map fliplr rts)
                      (map flipud rts)
                      (map (comp fliplr flipud) rts)))))

(defn transpose
  [mat]
  (apply map vector mat))

(defn n-by-ns
  [n mat]
  (if (= n (count mat))
    [mat]
    (->> mat
         (map #(partition n %))
         (partition n)
         (mapcat transpose))))

(defn split
  "Split mat into 2x2 or 3x3 mats."
  [mat]
  (let [n (count mat)]
    (condp = 0
      (mod n 2) (n-by-ns 2 mat)
      (mod n 3) (n-by-ns 3 mat))))

(defn join
  "Join mat parts into single mat."
  [parts]
  (let [n (count (flatten parts))
        i (count (flatten (first parts)))
        j (long (Math/sqrt (/ n i)))]
    (->> parts
         (partition j)
         (mapcat transpose)
         (map flatten))))

(def grow-part
  "Given an input mat and rules, get the first output w/ matching (potentially
  transformed) input."
  (memoize
   (fn [rules mat]
     (let [mats (transforms mat)]
       (->> mats
            (map #(get rules %))
            (filter some?)
            first)))))

(defn grow-parts
  [k rules mat]
  (loop [k k
         mat mat]
    (if (zero? k)
      mat
      (recur (dec k)
             (->> mat
                  split
                  (map #(grow-part rules %))
                  join)))))

(defn num-on
  "Number of pixels on after k iterations."
  [k rules mat]
  (->> (grow-parts k rules mat)
       flatten
       (filter #(= \# %))
       count))


;;; Part 2

;; How many pixels stay on after 18 iterations?

;;;; Performance analysis:

;; Memoization of `grow-part` really matters as number of parts grows larger;
;; for k = 18:

; - Unmemoized: "Elapsed time: 91388.995114 msecs"
;;- Memoized: "Elapsed time: 54146.969255 msecs"

;; We could also memoize `transforms`, but since it's only called in
;; `grow-part`, this wouldn't give any performance improvement.

;; Less naive implementation of rot90 seems statistically insignificant:

;; "Elapsed time: 50822.196629 msecs"

;; Parallelism for `grow-parts` doesn't seem to help; probably b/c each
;; `grow-part` is cheap, especially after memoization

;; "Elapsed time: 54922.680273 msecs"

;;;; Closing thoughts:

;; Split and join are likely MUCH more efficient w/ NumPy arrays or MATLAB
;; matrices than Clojure vectors / lazy seqs.

;; MATLAB has built-ins for `fliplr`, `flipup`, `rot90`, splitting into
;; blocks, and a very expressive syntax of joins: [A B; C D]. I'm sure NumPy
;; has equivalent functionality, though perhaps w/ less convenient
;; syntax. Python users have an advantage w/ this problem.

;; I considered using core.matrix, but didn't want to pay the learning cost
;; now.
