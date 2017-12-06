(ns aoc2017.day6-test
  (:require [aoc2017.day6 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 6.

  https://adventofcode.com/2017/day/6/input"
  (slurp (io/resource "day6-input.txt")))

(deftest num-until-cycle-test
  (is (= 5 (sut/num-until-cycle [0 2 7 0]))))

(deftest part-1-test
  (is (= 6681 (sut/num-until-cycle (sut/parse-input input)))))

(deftest period-test
  (is (= 4 (sut/period [0 2 7 0]))))

(deftest part-2-test
  (is (= 2392 (sut/period (sut/parse-input input)))))
