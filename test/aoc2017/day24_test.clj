(ns aoc2017.day24-test
  (:require [aoc2017.day24 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 24."
  (slurp (io/resource "day24-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 24.

  https://adventofcode.com/2017/day/24/input"
  (slurp (io/resource "day24-input.txt")))

(deftest strongest-bridge-strength-test
  (is (= 31 (sut/strongest-bridge-strength (sut/parse-input example)))))

(deftest part-1-test
  (is (= 1656 (sut/strongest-bridge-strength (sut/parse-input input)))))

(deftest strongest-longest-bridge-strength-test
  (is (= 19 (sut/strongest-longest-bridge-strength (sut/parse-input example)))))

(deftest part-2-test
  (is (= 1642 (sut/strongest-longest-bridge-strength (sut/parse-input input)))))
