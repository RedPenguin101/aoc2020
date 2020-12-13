(ns day11-2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-async-profiler.core :as prof]))

(defn parse-input [string]
  (mapv #(mapv {\L :empty \. :floor} %) (str/split-lines string)))

(defn seats [input]
  {:empty (set (for [[row-num row] (map-indexed vector input)
                     [col-num status] (map-indexed vector row)
                     :when (= status :empty)]
                 [row-num col-num]))})

(def ex1 (seats (parse-input (slurp "resources/day11example"))))
(def input (seats (parse-input (slurp "resources/day11input"))))

(defn neighbors-calc [[x y]]
  (set (map #(vector (+ x (% 0)) (+ y (% 1))) [[-1 -1] [0 -1] [1 -1]
                                               [-1 0] [1 0]
                                               [-1 1] [0 1] [1 1]])))

(defn new-state [neighbors-calc {:keys [empty occupied]}]
  (let [new-occ (set (filter #(zero? (count (set/intersection occupied (neighbors-calc %)))) empty))
        new-empty (set (filter #(>= (count (set/intersection occupied (neighbors-calc %))) 4) occupied))
        stay-occ (set/difference occupied new-empty)
        stay-empt (set/difference empty new-occ)]
    {:occupied (set/union new-occ stay-occ)
     :empty (set/union new-empty stay-empt)}))

(defn pp [{:keys [occupied empty]}]
  (let [x (into {} (for [c empty] [c :empty]))
        y (into {} (for [c occupied] [c :occupied]))
        seats (merge x y)
        ks (keys seats)
        cols (inc (apply max (map first ks)))
        rows (inc (apply max (map second ks)))]
    (map #(apply str %) (partition cols (map {:empty \L nil \. :occupied \#}
                                             (for [a (range cols)
                                                   b (range rows)]
                                               (get seats [a b])))))))

(comment
  (time (->> input
             (iterate (partial new-state neighbors-calc))
             (partition 2 1)
             (drop-while #(not= (first %) (second %)))
             ffirst
             :occupied
             count)))