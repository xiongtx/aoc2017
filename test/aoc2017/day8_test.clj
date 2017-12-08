(ns aoc2017.day8-test
  (:require [aoc2017.day8 :as sut]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 8."
  (slurp (io/resource "day8-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 8.

  https://adventofcode.com/2017/day/8/input"
  (slurp (io/resource "day8-input.txt")))

(deftest execute-test
  (is (= 1 (sut/execute (sut/parse-input example)))))

(deftest part-1-test
  (is (= 7787 (sut/execute (sut/parse-input input)))))

(deftest highest-value-test
  (is (= 10 (sut/highest-value (sut/parse-input example)))))

(deftest part-2-test
  (is (= 8997 (sut/highest-value (sut/parse-input input)))))
