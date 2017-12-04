(ns aoc2017.day3-test
  (:require [aoc2017.day3 :as sut]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 3.

  https://adventofcode.com/2017/day/3/input"
  265149)

(deftest distance-test
  (is (= 0 (sut/distance 1)))
  (is (= 3 (sut/distance 12)))
  (is (= 2 (sut/distance 23)))
  (is (= 31 (sut/distance 1024))))

(deftest part-1-test
  (is (= 438 (sut/distance input))))

(deftest sum-larger-than-test
  (is (= 23 (sut/sum-larger-than 20)))
  (is (= 304 (sut/sum-larger-than 300)))
  (is (= 806 (sut/sum-larger-than 800))))

(deftest part-2-test
  (is (= 266330 (sut/sum-larger-than input))))
