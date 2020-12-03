(ns day3
  (:require [clojure.string :as str]))

(defn next-row [a b]
  (fn [[x y]] [(+ a x) (+ b y)]))

(comment
  (take 5 (iterate (next-row 1 3) [0 0]))

  (str/split-lines (slurp "resources/day3test"))
  (map (fn [row] (map #(when (= \# %) :tree) row)) (str/split-lines (slurp "resources/day3test"))))

(defn parse-input [string]
  (mapv (fn [row] (mapv #(when (= \# %) :tree) row)) (str/split-lines string)))

(def test-input
  (parse-input (slurp "resources/day3test")))

(def input
  (parse-input (slurp "resources/day3input")))

(comment 
  (count input)
;; => 323
  (count (first input)))
;; => 31

(defn has-tree? [input [x y]]
  (get-in input [x (mod y (count (first input)))] :out-of-bounds))

(comment
  (take 12 (iterate (next-row 1 3) [0 0]))

  (take-while #(not= % :out-of-bounds) (map (partial has-tree? test-input) [[0 0] [1 3] [2 6] [3 9] [4 12] [5 15] [6 18] [7 21] [8 24] [9 27] [10 30] [11 33] [12 36]]))

  (count (remove nil? (take-while #(not= % :out-of-bounds) (map (partial has-tree? test-input) (iterate (next-row 1 3) [0 0])))))

  (->> [0 0]
       (iterate (next-row 1 3))
       (map (partial has-tree? test-input))
       (take-while #(not= % :out-of-bounds))
       (remove nil?)
       count))

(defn count-trees [input down-jump right-jump]
  (count (into [] (comp (map (partial has-tree? input))
                        (take-while #(not= % :out-of-bounds))
                        (remove nil?))
               (iterate (next-row down-jump right-jump) [0 0]))))

(comment 
  (count-trees test-input 1 3)
  
  "Part 1"
  (count-trees input 1 3)

  (has-tree? input [1 3])

  (get-in input [1 3])

  "Part 2"
  (apply * (for [jumps [[1 1] [1 3] [1 5] [1 7] [2 1]]]
             (apply count-trees input jumps))))

(defn count-trees2 [input down-jump right-jump]
  (count (into [] (comp (keep (partial has-tree? input))
                        (take-while #(not= % :out-of-bounds)))
               (iterate (next-row down-jump right-jump) [0 0]))))

(comment 
  (count-trees2 input 1 3)
  (apply * (for [jumps [[1 1] [1 3] [1 5] [1 7] [2 1]]]
             (apply count-trees2 input jumps))))