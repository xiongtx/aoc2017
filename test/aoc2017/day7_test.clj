(ns aoc2017.day7-test
  (:require [aoc2017.day7 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example
  "Example input from Advent of Code, Day 7."
  (slurp (io/resource "day7-example.txt")))

(def input
  "Puzzle input from Advent of Code, Day 7.

  https://adventofcode.com/2017/day/7/input"
  (slurp (io/resource "day7-input.txt")))

(deftest bottom-test
  (let [[g] (sut/parse-input example)]
    (is (= "tknk" (sut/bottom g)))))

(deftest part-1-test
  (let [[g] (sut/parse-input input)]
    (is (= "vmpywg" (sut/bottom g)))))

(deftest correct-weight-test
  (let [[g ws] (sut/parse-input example)]
    (is (= {:name "ugml"
            :weight 68
            :correct-weight 60} (sut/corrected g ws)))))

(deftest part-2-test
  (let [[g ws] (sut/parse-input input)
        {:keys [correct-weight]} (sut/corrected g ws)]
    (is (= 1674 correct-weight))))
