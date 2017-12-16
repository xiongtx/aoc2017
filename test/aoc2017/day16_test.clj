(ns aoc2017.day16-test
  (:require [aoc2017.day16 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 16.

  https://adventofcode.com/2017/day/16/input"
  (slurp (io/resource "day16-input.txt")))

(def example-programs (vec "abcde"))

(def programs (mapv char (range 97 113)))

(deftest dance-test
  (is (= (vec "baedc")
         (sut/dance example-programs (sut/parse-input "s1,x3/4,pe/b")))))

(deftest part-1-test
  (is (= (vec "gkmndaholjbfcepi")
         (sut/dance programs (sut/parse-input input)))))

(deftest part-2-test
  (is (= (vec "abihnfkojcmegldp")
         (sut/dance-billion programs (sut/parse-input input)))))
