(ns day11-2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-async-profiler.core :as prof]))

(defn parse-input [string]
  (mapv #(mapv {\L :empty \. :floor} %) (str/split-lines string)))

(def ex1 (parse-input (slurp "resources/day11example")))

(defn seats [input]
  {:empty (set (for [[row-num row] (map-indexed vector input)
                     [col-num status] (map-indexed vector row)
                     :when (= status :empty)]
                 [row-num col-num]))})

(def input (seats (parse-input (slurp "resources/day11input"))))

(defn neighbors-calc [[x y]]
  (set (map #(vector (+ x (% 0)) (+ y (% 1))) [[-1 -1] [0 -1] [1 -1]
                                               [-1 0] [1 0]
                                               [-1 1] [0 1] [1 1]])))

(defn new-state [{:keys [empty occupied]} neighbors]
  (let [new-occ (set (filter #(zero? (count (set/intersection occupied (neighbors %)))) empty))
        new-empty (set (filter #(>= (count (set/intersection occupied (neighbors %))) 4) occupied))
        stay-occ (set/difference occupied new-empty)
        stay-empt (set/difference empty new-occ)]
    {:occupied (set/union new-occ stay-occ)
     :empty (set/union new-empty stay-empt)}))

(defn iterate-to-stability [state neighbors it]
  (let [state' (new-state state neighbors)]
    (cond (= state state') state
          (> it 1000) :break
          :else (recur state' neighbors (inc it)))))


(comment 
  (count (:occupied (iterate-to-stability (seats ex1) (into {} (map #(vector % (neighbors-calc %)) (:empty (seats ex1)))) 0)))

  (time (count (:occupied (iterate-to-stability input (into {} (map #(vector % (neighbors-calc %)) (:empty input))) 0))))
  (prof/profile (count (:occupied (iterate-to-stability input (into {} (map #(vector % (neighbors-calc %)) (:empty input))) 0))))
  )


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

(time (count (:occupied (iterate-to-stability input (into {} (map #(vector % (neighbors-calc %)) (:empty input))) 0))))
