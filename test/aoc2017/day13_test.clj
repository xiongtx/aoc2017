(ns aoc2017.day13-test
  (:require [aoc2017.day13 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 13."
  (slurp (io/resource "day13-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 13.

  https://adventofcode.com/2017/day/13/input"
  (slurp (io/resource "day13-input.txt")))

(deftest severity-test
  (is (= 24 (sut/severity (sut/parse-input example)))))

(deftest part-1-test
  (is (= 1904 (sut/severity (sut/parse-input input)))))

(deftest delay-time-test
  (is (= 10 (sut/delay-time (sut/parse-input example)))))

(deftest part-2-test
  (is (= 3833504 (sut/delay-time (sut/parse-input input)))))
