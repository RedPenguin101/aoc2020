(ns aoc2020.day11-td
  (:require [clojure.string :as str]))

(def input
  (str/split-lines (slurp "resources/day11input")))

(def width
  (count (first input)))

(def height
  (count input))

(def grid
  (into {}
        (for [x (range width)
              y (range height)]
          [[x y] (nth (get input y) x)])))

(defn neighbours [[x y :as _point]]
  (for [[i j] [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]]
    [(+ x i) (+ y j)]))

(defn new-state [state]
  (into {}
        (for [x (range width)
              y (range height)
              :let [curr (get state [x y])]
              :when curr
              :let [count-occupied (count (filter (fn [p] (= (get state p) \#)) (neighbours [x y])))]]
          [[x y]
           (cond (and (= curr \L) (zero? count-occupied)) \#
                 (and (= curr \#) (>= count-occupied 4)) \L
                 :else curr)])))

(defn first-duplicate [coll]
  (reduce
   (fn [seen item]
     (if (seen item)
       (reduced item)
       (conj seen item)))
   #{}
   coll))

(defn first-duplicate-with-count [coll]
  (reduce
   (fn [{:keys [seen count]} item]
     (if (seen item)
       (reduced {:item item :count count})
       {:count (inc count)
        :seen (conj seen item)}))
   {:count 0
    :seen #{}}
   coll))

(comment
  (time (->> (iterate new-state grid)
             first-duplicate
             vals
             (filter #{\#})
             count))

  (->> (iterate new-state grid)
       first-duplicate-with-count
       :count)

  (time (->> (iterate new-state grid)
             first-duplicate-with-count
             :item
             vals
             (filter #{\#})
             count))


  1)