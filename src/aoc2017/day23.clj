(ns aoc2017.day23
  (:require [clojure.string :as str]))


;;; Part 1

;; You decide to head directly to the CPU and fix the printer from there. As
;; you get close, you find an experimental coprocessor doing so much work that
;; the local programs are afraid it will halt and catch fire. This would cause
;; serious issues for the rest of the computer, so you head in and see what
;; you can do.

;; The code it's running seems to be a variant of the kind you saw recently on
;; that tablet. The general functionality seems very similar, but some of the
;; instructions are different:

;; - set X Y sets register X to the value of Y.

;; - sub X Y decreases register X by the value of Y.

;; - mul X Y sets register X to the result of multiplying the value contained
;;   in register X by the value of Y.

;; - jnz X Y jumps with an offset of the value of Y, but only if the value of
;;   X is not zero. (An offset of 2 skips the next instruction, an offset of
;;   -1 jumps to the previous instruction, and so on.)

;; Only the instructions listed above are used. The eight registers here,
;; named a through h, all start at 0.

;; The coprocessor is currently set to some kind of debug mode, which allows
;; for testing, but prevents it from doing any meaningful work.

;; If you run the program (your puzzle input), how many times is the mul
;; instruction invoked?

(defn parse-line
  [line]
  (let [[_ ins x y] (re-find #"(\w+) +(\w+) *(-?\w+)?" line)
        x (try (Long/parseLong x) (catch Exception _ x))
        y (try (Long/parseLong y) (catch Exception _ y))]
    (case ins
      "snd" [ins x]
      "rcv" [ins x]
      [ins x y])))

(defn parse-input
  [input]
  (->> input
       str/trim-newline
       str/split-lines
       (map parse-line)))

(def registers #{"a" "b" "c" "d" "e" "f" "g" "h"})

(def initial-state-debug (into {} (map #(vector % 0) registers)))

(defn finished?
  [pt n]
  (or (neg? pt) (>= pt n)))

(defn get-val [state x]
  (if (number? x) x (get state x)))

(defn num-mul
  "Return number of times mul is called."
  [instructions]
  (let [n (count instructions)]
    (loop [state initial-state-debug
           pt 0
           k 0]
      (if (finished? pt n)
        k
        (let [[ins x y] (nth instructions pt)
              y (get-val state y)]
          (case ins
            "jnz" (recur state
                         (if (not (zero? (get-val state x)))
                           (+ pt y)
                           (inc pt))
                         k)
            "mul" (recur (update state x * y) (inc pt) (inc k))
            "set" (recur (assoc state x y) (inc pt) k)
            "sub" (recur (update state x - y) (inc pt) k)))))))


;;; Part 2

;; Now, it's time to fix the problem.

;; The debug mode switch is wired directly to register a. You flip the switch,
;; which makes register a now start at 1 when the program is executed.

;; Immediately, the coprocessor begins to overheat. Whoever wrote this program
;; obviously didn't choose a very efficient implementation. You'll need to
;; optimize the program if it has any hope of completing before Santa needs
;; that printer working.

;; The coprocessor's ultimate goal is to determine the final value left in
;; register h once the program completes. Technically, if it had that... it
;; wouldn't even need to run the program.

;; After setting register a to 1, if the program were to run to completion,
;; what value would be left in register h?


;; For this problem, it's important to understand what the assembly input
;; does.

;; See: https://www.reddit.com/r/adventofcode/comments/7lms6p/2017_day_23_solutions/drnhcx8/

(defn prime?
  [x]
  (->> (map #(mod x %) (range 2 (inc (Math/sqrt x))))
       (not-any? zero?)))

(defn h-value
  "Return the value of h when program exits."
  []
  (let [b 109300
        c (+ b 17000)]
    (->> (range b (inc c) 17)
         (filter (complement prime?))
         count)))
