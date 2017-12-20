(ns aoc2017.day20-test
  (:require [aoc2017.day20 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 20."
  (slurp (io/resource "day20-example.txt")))

(def example2
  "Part 2's example input from Advent of Code, Day 20."
  (slurp (io/resource "day20-example2.txt")))

(def input
  "Puzzle input from Advent of Code, Day 20.

  https://adventofcode.com/2017/day/18/input"
  (slurp (io/resource "day20-input.txt")))

(deftest closest-test
  (is (= 0 (sut/closest (sut/parse-input example)))))

(deftest part-1-test
  (is (= 157 (sut/closest (sut/parse-input input)))))

(deftest num-noncolliding-test
  (is (= 1 (sut/num-noncolliding (sut/parse-input example2)))))

(deftest part-2-test
  (is (= 499 (sut/num-noncolliding (sut/parse-input input)))))
