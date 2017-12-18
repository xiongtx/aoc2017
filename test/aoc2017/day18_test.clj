(ns aoc2017.day18-test
  (:require [aoc2017.day18 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 18."
  (slurp (io/resource "day18-example.txt")))

(def example2
  "Part 2's example input from Advent of Code, Day 18."
  (slurp (io/resource "day18-example2.txt")))

(def input
  "Puzzle input from Advent of Code, Day 18.

  https://adventofcode.com/2017/day/18/input"
  (slurp (io/resource "day18-input.txt")))

(deftest play-test
  (is (= 4 (sut/play (sut/parse-input example)))))

(deftest part-1-test
  (is (= 3423 (sut/play (sut/parse-input input)))))

(deftest both-play-test
  (is (= 3 (sut/both-play (sut/parse-input example2)))))

(deftest part-2-test
  (is (= 7493 (sut/both-play (sut/parse-input input)))))
