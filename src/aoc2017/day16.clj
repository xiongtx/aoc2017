(ns aoc2017.day16
  (:require [clojure.string :as str]))


;;; Part 1

;; You come upon a very unusual sight; a group of programs here appear to be
;; dancing.

;; There are sixteen programs in total, named a through p. They start by
;; standing in a line: a stands in position 0, b stands in position 1, and so
;; on until p, which stands in position 15.

;; The programs' dance consists of a sequence of dance moves:

;; - Spin, written sX, makes X programs move from the end to the front, but
;;   maintain their order otherwise. (For example, s3 on abcde produces
;;   cdeab).

;; - Exchange, written xA/B, makes the programs at positions A and B swap
;;   places.

;; - Partner, written pA/B, makes the programs named A and B swap places.

;; For example, with only five programs standing in a line (abcde), they could
;; do the following dance:

;; - s1, a spin of size 1: eabcd.

;; - x3/4, swapping the last two programs: eabdc.

;; - pe/b, swapping programs e and b: baedc.

;; After finishing their dance, the programs end up in order baedc.

;; You watch the dance for a while and record their dance moves (your puzzle
;; input). In what order are the programs standing after their dance?

(defn parse-move
  [move]
  (let [c (first move)
        s (subs move 1)]
    (case c
      \s [c (Long/parseLong s)]
      \x (let [[_ i j] (re-find #"(\d+)/(\d+)" s)]
           [c (Long/parseLong i) (Long/parseLong j)])
      \p [c (nth s 0) (nth s 2)])))

(defn parse-input
  [input]
  (->> (-> input
           str/trim-newline
           (str/split #","))
       (mapv parse-move)))

(defn spin
  [v k]
  (let [n (count v)]
    (->> (cycle v)
         (drop (- n (mod k n)))
         (take n)
         vec)))

(defn exchange
  [v i j]
  (let [x (nth v i)
        y (nth v j)]
    (-> v
        (assoc i y)
        (assoc j x))))

(defn partner
  [v x y]
  (let [i (.indexOf v x)
        j (.indexOf v y)]
    (-> v
        (assoc i y)
        (assoc j x))))

(defn dance-move
  [v move]
  (let [[c a b] move]
    (case c
      \s (spin v a)
      \x (exchange v a b)
      \p (partner v a b))))

(defn dance
  [v moves]
  (reduce dance-move v moves))


;;; Part 2

;; Now that you're starting to get a feel for the dance moves, you turn your
;; attention to the dance as a whole.

;; Keeping the positions they ended up in from their previous dance, the
;; programs perform it again and again: including the first dance, a total of
;; one billion (1000000000) times.

;; In the example above, their second dance would begin with the order baedc,
;; and use the same dance moves:

;; - s1, a spin of size 1: cbaed.
;; - x3/4, swapping the last two programs: cbade.
;; - pe/b, swapping programs e and b: ceadb.

;; In what order are the programs standing after their billion dances?

(defn period
  "Number of times dance moves are applied until program returns to previously
  seen position."
  [v moves]
  (loop [i 0
         v v
         vs #{(str/join v)}]
    (let [w (dance v moves)]
      (if (contains? vs w)
        i
        (recur (inc i) w (conj vs w))))))

(defn dance-billion
  [v moves]
  (loop [i (mod 1E9 (period v moves))
         v v]
    (if (zero? i)
      v
      (recur (dec i) (dance v moves)))))
