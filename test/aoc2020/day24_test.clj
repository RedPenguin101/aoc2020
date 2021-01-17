(ns aoc2020.day24-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2020.day24 :refer [hex]]))

(deftest a
  (is (= [0 0 0]
         (hex "nwwswee")))
  (is (= [0 -1 1]
         (hex "esew"))))
