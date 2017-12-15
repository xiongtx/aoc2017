(ns aoc2017.day15-test
  (:require [aoc2017.day15 :as sut]
            [clojure.test :refer :all]))

(def example-a 65)
(def example-b 8921)

(def input-a 703)
(def input-b 516)

(deftest num-equal-test
  (is (= 1 (sut/num-equal example-a example-b 5)))
  (is (= 588 (sut/num-equal example-a example-b 40E6))))

(deftest part-1-test
  (is (= 594 (sut/num-equal input-a input-b 40E6))))

(deftest num-equal-picky-test
  (is (= 1 (sut/num-equal-picky example-a example-b 1056)))
  (is (= 309 (sut/num-equal-picky example-a example-b 5E6))))

(deftest part-2-test
  (is (= 328 (sut/num-equal-picky input-a input-b 5E6))))
