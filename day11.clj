(ns day11
  (:require [clojure.string :as str]
            [clj-async-profiler.core :as prof]))

(set! *warn-on-reflection* true)

(defn parse-input [string]
  (mapv #(mapv {\L :seat \. :floor} %) (str/split-lines string)))

(defn seat-coords [input]
  (into {} (remove nil? (for [[row-num row] (map-indexed vector input)
                              [seat-num seat] (map-indexed vector row)]
                          (when (= :seat seat) [[row-num seat-num] :empty])))))

(def seats (into {} (remove nil? (seat-coords (parse-input (slurp "resources/day11example"))))))
(def input (into {} (remove nil? (seat-coords (parse-input (slurp "resources/day11input"))))))

(defn neighbors [[x y]]
  (mapv #(vector (+ x (% 0)) (+ y (% 1))) [[-1 -1] [0 -1] [1 -1]
                                           [-1 0] [1 0]
                                           [-1 1] [0 1] [1 1]]))

(defn occupied-neighbours [seats this]
  (count (filter #{:occupied} (keep seats (neighbors this)))))

(defn oc [seats]
  (reduce (fn [A coord] (assoc A coord (occupied-neighbours seats coord)))
          {}
          (keys seats)))

(defn new-state [seats]
  (let [o (oc seats)]
    (reduce (fn [A [coord status]]
              (assoc A coord
                     (cond (and (= :empty status) (zero? (o coord)))
                           :occupied

                           (and (= :occupied status) (>= (o coord) 4))
                           :empty

                           :else  status)))
            {}
            seats)))

(defn iterate-to-stability [it seats]
  (let [seats' (new-state seats)]
    (cond (= seats' seats) seats
          (> it 1000) :break
          :else (recur (inc it) seats'))))

(defn pp [seats]
  (let [ks (keys seats)
        cols (inc (apply max (map first ks)))
        rows (inc (apply max (map second ks)))]
    (map #(apply str %) (partition cols (map {:empty \L nil \. :occupied \#}
                                             (for [a (range cols)
                                                   b (range rows)]
                                               (get seats [a b])))))))

(comment
  (time (->> input
             (iterate-to-stability 0)
             vals
             (filter #{:occupied})
             count))
  (prof/profile (->> input
                     (iterate-to-stability 0)
                     vals
                     (filter #{:occupied})
                     count)))

