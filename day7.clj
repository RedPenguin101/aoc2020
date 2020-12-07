(ns day7
  (:require [clojure.string :as str]
            [ubergraph.core :as uber]))

(defn inner-parse [i]
  (let [[qty clr] (rest (re-matches #"(\d) (\w* \w*) bags?\.?" i))]
    [clr (Integer/parseInt qty)]))

(defn parse-rule [i]
  (let [[key val] (str/split i #" bags contain ")]
    (when (not= val "no other bags.")
      [key (into {} (map (comp inner-parse str/trim) (str/split val #",")))])))

(defn parse-input [filename]
  (into {} (map parse-rule (str/split-lines filename))))

(defn build-graph [bag-map]
  (apply uber/digraph
         (for [[start ends] bag-map
               [color quantity] ends]
           [start color quantity])))

(defn find-all-parents [graph colors]
  (let [immediate-parents (set (concat colors (mapcat #(map :src (uber/find-edges graph {:dest %})) colors)))]
    (if (= immediate-parents colors)
      immediate-parents
      (recur graph immediate-parents))))

(defn child-bags [graph bag-type]
  (let [children (map :dest (uber/find-edges graph {:src bag-type}))]
    (->> children
         (map #(:weight (last (uber/edge-with-attrs graph [bag-type %]))))
         (mapv vector children))))

(defn total-bags [graph bag-type]
  (->> bag-type 
       (child-bags graph)
       (map (fn [[child-bag-type n]] (* n (+ 1 (total-bags graph child-bag-type)))))
       (reduce +)))

(comment
  "Core data representation of rules"
  
  (into {} (map parse-rule (str/split-lines (slurp "resources/day7example"))))
  (into {} (map parse-rule (str/split-lines (slurp "resources/day7input"))))

  (def example {"light red"    {"bright white" 1, "muted yellow" 2}
                "dark orange"  {"bright white" 3, "muted yellow" 4}
                "bright white" {"shiny gold" 1}
                "muted yellow" {"shiny gold" 2, "faded blue" 9}
                "shiny gold"   {"dark olive" 1, "vibrant plum" 2}
                "dark olive"   {"faded blue" 3, "dotted black" 4}
                "vibrant plum" {"faded blue" 5, "dotted black" 6}})

  (def example-graph (build-graph example))

  (find-all-parents example-graph #{"shiny gold"})

  "Part 1 solution"
  
  (dec (count (find-all-parents
               (build-graph (parse-input (slurp "resources/day7input")))
               #{"shiny gold"})))
  ;; => 332
  
  (child-bags example-graph "dark olive")

  (total-bags example-graph "shiny gold")

  (total-bags
   (build-graph (parse-input (slurp "resources/day7example")))
   "shiny gold")

  "Part 2 solution"
  
  (total-bags
   (build-graph (into {} (map parse-rule (str/split-lines (slurp "resources/day7input")))))
   "shiny gold")
  ;; => 10875
  
  (and (= 10875 (total-bags
                 (build-graph (parse-input (slurp "resources/day7input")))
                 "shiny gold"))
       (= 332 (dec (count (find-all-parents
                           (build-graph (parse-input (slurp "resources/day7input")))
                           #{"shiny gold"}))))))