(ns aoc2017.day4-test
  (:require [aoc2017.day4 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 4.

  https://adventofcode.com/2017/day/4/input"
  (slurp (io/resource "day4-input.txt")))

(deftest num-valid-test
  (is (= 1 (sut/num-valid "aa bb cc dd ee")))
  (is (zero? (sut/num-valid "aa bb cc dd aa")))
  (is (= 1 (sut/num-valid "aa bb cc dd aaa"))))

(deftest part-1-test
  (is (= 455 (sut/num-valid input))))

(deftest part-2-test
  (is (= 186 (sut/num-valid-2 input))))
