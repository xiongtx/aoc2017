(ns aoc2017.day8
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))


;;; Part 1

;; You receive a signal directly from the CPU. Because of your recent
;; assistance with jump instructions, it would like you to compute the result
;; of a series of unusual register instructions.

;; Each instruction consists of several parts: the register to modify, whether
;; to increase or decrease that register's value, the amount by which to
;; increase or decrease it, and a condition. If the condition fails, skip the
;; instruction without modifying the register. The registers all start at
;; 0. The instructions look like this:

;; b inc 5 if a > 1
;; a inc 1 if b < 5
;; c dec -10 if a >= 1
;; c inc -20 if c == 10

;; These instructions would be processed as follows:

;; - Because a starts at 0, it is not greater than 1, and so b is not
;;   modified.

;; - a is increased by 1 (to 1) because b is less than 5 (it is 0).

;; - c is decreased by -10 (to 10) because a is now greater than or equal to
;;   1 (it is 1).

;; - c is increased by -20 (to -10) because c is equal to 10.

;; - After this process, the largest value in any register is 1.

;; You might also encounter <= (less than or equal to) or != (not equal
;; to). However, the CPU doesn't have the bandwidth to tell you what all the
;; registers are named, and leaves that to you to determine.

;; What is the largest value in any register after completing the instructions
;; in your puzzle input?

(defn parse-line
  [line]
  (let [[reg op val _ reg2 op2 val2] (str/split line #" ")
        op (case op
             "inc" +
             "dec" -)
        op2 (case op2
              ">" >
              "<" <
              ">=" >=
              "<=" <=
              "==" ==
              "!=" not=)]
    [reg op (Long/parseLong val) reg2 op2 (Long/parseLong val2)]))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv parse-line)))

(defn execute
  [instructions]
  (->> instructions
       (reduce (fn [registers [reg op val reg2 op2 val2]]
                 (if (op2 (get registers reg2 0) val2)
                   (update registers reg (fnil op 0) val)
                   registers))
               {})
       vals
       (apply max)))


;;; Part 2

;; To be safe, the CPU also needs to know the highest value held in any
;; register during this process so that it can decide how much memory to
;; allocate to these operations. For example, in the above instructions, the
;; highest value ever held was 10 (in register c after the third instruction
;; was evaluated).

(defn highest-value
  [instructions]
  (loop [highest 0
         ins instructions
         registers {}]
    (if (empty? ins)
      highest
      (let [[reg op val reg2 op2 val2] (first ins)]
        (if (op2 (get registers reg2 0) val2)
          (let [k (op (get registers reg 0) val)]
            (recur (max highest k)
                   (rest ins)
                   (assoc registers reg k)))
          (recur highest (rest ins) registers))))))
