(ns day10
  (:require [clojure.string :as str]))

(def ex1 [16 10 15 5 1 11 7 19 6 12 4])
(def ex2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])
(def input (->> "resources/day10input"
                slurp
                str/split-lines
                (map #(Long/parseLong %))))

(defn complete-input [input]
  (sort (conj (conj input 0) (+ 3 (apply max input)))))

(defn joltage-differences [input]
  (frequencies (map #(- (apply - %)) (partition 2 1 (complete-input input)))))

(joltage-differences ex1)
;; => {1 7, 3 5}
(joltage-differences ex2)
;; => {1 22, 3 10}

(->> input
     joltage-differences
     vals
     (apply *))
;; => 2170

(defn find-predecessors [[fst & rst]]
  [fst (filter #(> % (- fst 4)) rst)])

(defn distinct-paths [input]
  (->> input
       complete-input
       reverse
       (partition-all 4 1)
       reverse
       (map find-predecessors)
       (drop 1)
       (reduce (fn [A [x ys]] (assoc A x (apply + (map #(get A %) ys)))) {0 1})))

(distinct-paths ex1)
(distinct-paths ex2)

(time (apply max (vals (distinct-paths input))))
;; => 24803586664192
