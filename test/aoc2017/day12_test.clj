(ns aoc2017.day12-test
  (:require [aoc2017.day12 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 12."
  (slurp (io/resource "day12-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 12.

  https://adventofcode.com/2017/day/12/input"
  (slurp (io/resource "day12-input.txt")))

(deftest group-size-test
  (is (= 6 (sut/group-size (sut/parse-input example) "0"))))

(deftest part-1-test
  (is (= 113 (sut/group-size (sut/parse-input input) "0"))))

(deftest num-groups-test
  (is (= 2 (sut/num-groups (sut/parse-input example)))))

(deftest part-2-test
  (is (= 202 (sut/num-groups (sut/parse-input input)))))
