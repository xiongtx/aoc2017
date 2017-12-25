(ns aoc2017.day25-test
  (:require [aoc2017.day25 :as sut]
            [clojure.test :refer :all]))

(def example-state-transitions
  {:A {0 [1 1 :B]
       1 [0 -1 :B]}
   :B {0 [1 -1 :A]
       1 [1 1 :A]}})

(deftest checksum-test
  (is (= 3 (sut/checksum 6 example-state-transitions))))

(deftest part-1-test
  (is (= 5744 (sut/checksum 12261543 sut/state-transitions))))
