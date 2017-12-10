(ns aoc2017.day10-test
  (:require [aoc2017.day10 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 10.

  https://adventofcode.com/2017/day/10/input"
  (slurp (io/resource "day10-input.txt")))

(deftest tie-knots-test
  (is (= [3 4 2 1 0]
         (-> (sut/tie-knots (sut/parse-input "3,4,1,5") (range 5))
             first))))

(deftest knot-hash-test
  (is (= 12 (sut/knot-hash (sut/parse-input "3,4,1,5") (range 5)))))

(deftest part-1-test
  (is (= 38628 (sut/knot-hash (sut/parse-input input)))))

(deftest dense-hash-test
  (are [e s] (= e (sut/dense-hash (sut/parse-input-bytes s)))
    "a2582a3a0e66e6e86e3812dcb672a272" ""
    "33efeb34ea91902bb2f59c9920caa6cd" "AoC 2017"
    "3efbe78a8d82f29979031a4aa0b16a9d" "1,2,3"
    "63960835bcdc130f0b66d7ff4f6a5a8e" "1,2,4"))

(deftest part-2-test
  (is (= "e1462100a34221a7f0906da15c1c979a"
         (sut/dense-hash (sut/parse-input-bytes input)))))
