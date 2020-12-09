(ns day9
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn int-parse [s]
  (try (Integer/parseInt s)
       (catch NumberFormatException e (BigInteger. s))))

(def input (->> "resources/day9input"
                slurp
                str/split-lines
                (map int-parse)))

(def example [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn not-sum-of-predecessors [xs]
  (when (not-any? #(#{(last xs)} (apply + %)) (combo/combinations (butlast xs) 2))
    (last xs)))

(comment
  (some not-sum-of-predecessors (partition (inc 5) 1 example))
  ;; => 127

  (some not-sum-of-predecessors (partition (inc 25) 1 input))
  ;; => 70639851
  )

(defn contiguous-block-summing-to
  ([target xs] (contiguous-block-summing-to target xs 0 1))
  ([target xs  start end]
   (let [x (apply + (subvec xs start end))]
     (cond (= x target) (subvec xs start end)
           (> x target) (recur target xs (inc start) end)
           (< x target) (recur target xs start (inc end))))))

(comment
  (contiguous-block-summing-to 127 example )
  ;; => [15 25 47 40]

  (let [r (sort (contiguous-block-summing-to 70639851 (vec input)))]
    (+ (first r) (last r)))
  ;; => 8249240
  )
