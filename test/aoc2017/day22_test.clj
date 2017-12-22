(ns aoc2017.day22-test
  (:require [aoc2017.day22 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 22."
  (slurp (io/resource "day22-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 22.

  https://adventofcode.com/2017/day/22/input"
  (slurp (io/resource "day22-input.txt")))

(deftest num-infecting-bursts-test
  (are [e m] (= e (sut/num-infecting-bursts m (sut/parse-input example)))
    5 7
    41 70
    5587 10000))

(deftest part-1-test
  (is (= 5462 (sut/num-infecting-bursts 10000 (sut/parse-input input)))))

(deftest num-infecting-bursts-evolved-test
  (are [e m] (= e (sut/num-infecting-bursts-evolved m (sut/parse-input example)))
    26 100
    2511944 10000000))

(deftest part-2-test
  (is (= 2512135
         (sut/num-infecting-bursts-evolved 10000000 (sut/parse-input input)))))
