(ns day7
  (:require [clojure.string :as str]
            [ubergraph.core :as uber]))

(defn parse-rule [i]
  (let [[parent children] (str/split i #"bags contain")]
    [(str/trim parent) (into {} (map (fn [[_ num node]] [node (Integer/parseInt num)]) (re-seq #"(\d) (\w+ \w+) bag" children)))]))

(defn parse-input [string]
  (into {} (map parse-rule (remove #(re-find #"no other bags" %) (str/split-lines string)))))

(defn find-all-parents [graph node-name]
  (when node-name
    (set (conj (mapcat #(find-all-parents graph %) (uber/predecessors graph node-name)) node-name))))

(defn total-bags [graph bag-type]
  (reduce + (for [child (uber/successors graph bag-type)] 
              (* (uber/weight graph bag-type child) (+ 1 (total-bags graph child))))))

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

  (def example-graph (uber/digraph example))

  (find-all-parents example-graph #{"shiny gold"})

  "Part 1 solution"

  (dec (count (find-all-parents
               (uber/digraph (parse-input (slurp "resources/day7input")))
               "shiny gold")))
  ;; => 332

  "Part 2 solution"

  (total-bags
   (uber/digraph (parse-input (slurp "resources/day7input")))
   "shiny gold")
  ;; => 10875
  )
  