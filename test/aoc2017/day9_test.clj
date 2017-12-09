(ns aoc2017.day9-test
  (:require [aoc2017.day9 :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def input
  "Puzzle input from Advent of Code, Day 9.

  https://adventofcode.com/2017/day/9/input"
  (slurp (io/resource "day9-input.txt")))

(deftest score-test
  (are [e s] (= e (sut/score s))
    1 "{}"
    6 "{{{}}}"
    5 "{{},{}}"
    16 "{{{},{},{{}}}}"
    1 "{<a>,<a>,<a>,<a>}"
    9 "{{<ab>},{<ab>},{<ab>},{<ab>}}"
    9 "{{<!!>},{<!!>},{<!!>},{<!!>}}"
    3 "{{<a!>},{<a!>},{<a!>},{<ab>}}"))

(deftest part-1-test
  (is (= 11089 (sut/score input))))

(deftest num-garbage-test
  (are [e s] (= e (sut/num-garbage s))
    0 "<>"
    3 "<abc>"
    3 "<<<<>"
    2 "<{!>}>"
    0 "<!!>"
    0 "<!!!>>"
    10 "<{o\"i!a,<{i<a>"))

(deftest part-2-test
  (is (= 5288 (sut/num-garbage input))))
