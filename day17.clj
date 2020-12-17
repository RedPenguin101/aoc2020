(ns day17
  (:require [clojure.string :as str]))

(defn- parse-input [input]
  (remove nil? (apply concat (map-indexed (fn bleh [idx row] (map-indexed #(if (= \# %2) [idx %1 0] nil) row)) (str/split-lines input)))))

(defn- parse-input-4d [input]
  (remove nil? (apply concat (map-indexed (fn bleh [idx row] (map-indexed #(if (= \# %2) [idx %1 0 0] nil) row)) (str/split-lines input)))))

(defn- neighbors [[x y z]]
  (for [xvar [-1 0 1]
        yvar [-1 0 1]
        zvar [-1 0 1]
        :when  (not= xvar yvar zvar 0)]
    [(+ x xvar) (+ y yvar) (+ z zvar)]))

(defn- neighbors-4d [[x y z w]]
  (for [xvar [-1 0 1]
        yvar [-1 0 1]
        zvar [-1 0 1]
        wvar [-1 0 1]
        :when  (not= xvar yvar zvar wvar 0)]
    [(+ x xvar) (+ y yvar) (+ z zvar) (+ w wvar)]))

(defn- step [neighbors coords]
  (let [stuff (reduce (fn [A [c freq]] (if (#{2 3} freq) (update A freq conj c) A)) {} (frequencies (apply concat (map neighbors coords))))]
    (concat (get stuff 3)
            (filter (set coords) (get stuff 2)))))

(comment
  (time (count (last (take 7 (iterate (partial step neighbors) (parse-input (slurp "resources/day17example")))))))
  ;; => 112
  (time (count (last (take 7 (iterate (partial step neighbors) (parse-input (slurp "resources/day17input")))))))
  ;; => 353


  (count (last (take 7 (iterate (partial step neighbors-4d) (parse-input-4d (slurp "resources/day17example"))))))
  ;; => 848
  (time (count (last (take 7 (iterate (partial step neighbors-4d) (parse-input-4d (slurp "resources/day17input")))))))
  ;; => 2472
)