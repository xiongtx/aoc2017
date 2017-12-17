(ns aoc2017.day17-test
  (:require [aoc2017.day17 :as sut]
            [clojure.test :refer :all]))

(def example-input 3)

(def input 377)

(deftest last-val-test
  (is (= 638 (sut/last-val 2017 example-input))))

(deftest part-1-test
  (is (= 596 (sut/last-val 2017 input))))

(deftest val-after-zero-test
  (are [e n] (= e (sut/val-after-zero n example-input))
    2 4
    5 5
    9 9))

(deftest part-2-test
  (is (= 39051595 (sut/val-after-zero 5E7 input))))
