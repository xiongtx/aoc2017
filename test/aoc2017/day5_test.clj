(ns aoc2017.day5-test
  (:require [aoc2017.day5 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 5.

  https://adventofcode.com/2017/day/5/input"
  (slurp (io/resource "day5-input.txt")))

(deftest steps-to-escape-test
  (is (= 5 (sut/steps-to-escape [0 3 0 1 -3]))))

(deftest part-1-test
  (is (= 378980 (sut/steps-to-escape (sut/parse-input input)))))

(deftest steps-to-escape-2-test
  (is (= 10 (sut/steps-to-escape-2 [0 3 0 1 -3]))))

(deftest part-2-test
  (is (= 26889114 (sut/steps-to-escape-2 (sut/parse-input input)))))
