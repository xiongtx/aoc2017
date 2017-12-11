(ns aoc2017.day11-test
  (:require [aoc2017.day11 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 11.

  https://adventofcode.com/2017/day/11/input"
  (slurp (io/resource "day11-input.txt")))

(deftest distance-test
  (are [e s] (= e (sut/final-distance (sut/parse-input s)))
    3 "ne,ne,ne"
    0 "ne,ne,sw,sw"
    2 "ne,ne,s,s"
    3 "se,sw,se,sw,sw"))

(deftest part-1-test
  (is (= 812 (sut/final-distance (sut/parse-input input)))))

(deftest part-2-test
  (is (= 1603 (sut/furthest-distance (sut/parse-input input)))))
