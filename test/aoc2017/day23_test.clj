(ns aoc2017.day23-test
  (:require [aoc2017.day23 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 23.

  https://adventofcode.com/2017/day/23/input"
  (slurp (io/resource "day23-input.txt")))

(deftest part-1-test
  (is (= 8281 (sut/num-mul (sut/parse-input input)))))

(deftest part-2-test
  (is (= 911 (sut/h-value))))
