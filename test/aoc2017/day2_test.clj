(ns aoc2017.day2-test
  (:require [aoc2017.day2 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 2.

  https://adventofcode.com/2017/day/2/input"
  (slurp (io/resource "day2-input.txt")))

(deftest checksum-test
  (let [spreadsheet "5 1 9 5
                     7 5 3
                     2 4 6 8"]
    (is (= 18 (sut/checksum spreadsheet)))))

(deftest part-1-test
  (is (= 43074 (sut/checksum input))))

(deftest checksum-divides-test
  (let [spreadsheet "5 9 2 8
                     9 4 7 3
                     3 8 6 5"]
    (is (= 9 (sut/checksum-divides spreadsheet)))))

(deftest part-2-test
  (is (= 280 (sut/checksum-divides input))))
