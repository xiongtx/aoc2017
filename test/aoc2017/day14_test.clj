(ns aoc2017.day14-test
  (:require [aoc2017.day14 :as sut]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 13."
  "flqrgnkx")

(def input
  "Puzzle input from Advent of Code, Day 14.

  https://adventofcode.com/2017/day/14/input"
  "hfdlxzhv")

(deftest num-used-test
  (is (= 8108 (sut/num-used-parallel example))))

(deftest part-1-test
  (is (= 8230 (sut/num-used-parallel input))))

(def example-grid (delay (sut/make-grid example)))

(def input-grid (delay (sut/make-grid input)))

(deftest num-regions-test
  (is (= 1242 (sut/num-regions @example-grid))))

(deftest part-2-test
  (is (= 1103 (sut/num-regions @input-grid))))
