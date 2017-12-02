(ns aoc2017.day2
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))


;;; Part 1

;; The spreadsheet consists of rows of apparently-random numbers. To make sure
;; the recovery process is on the right track, they need you to calculate the
;; spreadsheet's checksum. For each row, determine the difference between the
;; largest value and the smallest value; the checksum is the sum of all of
;; these differences.

;; For example, given the following spreadsheet:

;; 5 1 9 5
;; 7 5 3
;; 2 4 6 8

;; - The first row's largest and smallest values are 9 and 1, and their
;;   difference is 8.

;; - The second row's largest and smallest values are 7 and 3, and their
;;   difference is 4.

;; - The third row's difference is 6.

;; In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

(defn parse-input
  "Parse a string of numbers representing a spreadsheet."
  [input]
  (->> (str/split-lines input)
       (map #(str/split (str/trim %) #"\s+"))
       (walk/postwalk #(if (string? %) (Long/parseLong (str/trim %)) %))))

(defn checksum
  [input]
  (->> (parse-input input)
       (map #(- (apply max %) (apply min %)))
       (apply +)))


;;; Part 2

;; It sounds like the goal is to find the only two numbers in each row where
;; one evenly divides the other - that is, where the result of the division
;; operation is a whole number. They would like you to find those numbers on
;; each line, divide them, and add up each line's result.

;; For example, given the following spreadsheet:

;; 5 9 2 8
;; 9 4 7 3
;; 3 8 6 5

;; - In the first row, the only two numbers that evenly divide are 8 and 2;
;;   the result of this division is 4.

;; - In the second row, the two numbers are 9 and 3; the result is 3.

;; - In the third row, the result is 2.

;; In this example, the sum of the results would be 4 + 3 + 2 = 9.

(defn checksum-divides
  [input]
  (->> (for [row (parse-input input)
             a row
             b row
             :when (and (not= a b) (zero? (rem a b)))]
         (/ a b))
       (apply +)))
