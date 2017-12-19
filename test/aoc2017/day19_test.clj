(ns aoc2017.day19-test
  (:require [aoc2017.day19 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 19."
  (slurp (io/resource "day19-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 19.

  https://adventofcode.com/2017/day/18/input"
  (slurp (io/resource "day19-input.txt")))

(deftest travel-test
  (is (= "ABCDEF" (sut/travel (sut/parse-input example)))))

(deftest part-1-test
  (is (= "KGPTMEJVS" (sut/travel (sut/parse-input input)))))

(deftest num-steps-test
  (is (= 38 (sut/num-steps (sut/parse-input example)))))

(deftest part-2-test
  (is (= 16328 (sut/num-steps (sut/parse-input input)))))
