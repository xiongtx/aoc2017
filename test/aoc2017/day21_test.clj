(ns aoc2017.day21-test
  (:require [aoc2017.day21 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 21."
  (slurp (io/resource "day21-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 21.

  https://adventofcode.com/2017/day/21/input"
  (slurp (io/resource "day21-input.txt")))

(def rules-example (sut/parse-input example))

(def rules-input (sut/parse-input input))

(deftest num-on-test
  (is (= 12 (sut/num-on 2 rules-example sut/start))))

(deftest part-1-test
  (is (= 117 (sut/num-on 5 rules-input sut/start))))

(deftest part-2-test
  (is (= 2026963 (sut/num-on 18 rules-input sut/start))))
