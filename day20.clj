(ns day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-tile [tile-block]
  (let [[id-row & tile] (str/split-lines tile-block)
        tile (mapv vec tile)]
    {:id (Integer/parseInt (subs id-row 5 9))
     :tile tile
     :top (first tile)
     :bottom (last tile)
     :left (first (apply map vector tile))
     :right (last (apply map vector tile))}))

(def ex-tiles (map parse-tile (str/split (slurp "resources/day20example") #"\n\n")))
(def input (map parse-tile (str/split (slurp "resources/day20input") #"\n\n")))

(def tile-1951 (first (filter #(= 1951 (:id %)) ex-tiles)))
(def tile-2311 (first (filter #(= 2311 (:id %)) ex-tiles)))

(defn find-edge-matches 
  [tile1 tile2]
  (let [t2-edges (vals (select-keys tile2 [:left :right :top :bottom]))
        t2-edges (set (concat t2-edges (map reverse t2-edges)))]
    (set (remove nil? [(when (t2-edges (:left tile1)) :left)
                       (when (t2-edges (:right tile1)) :right)
                       (when (t2-edges (:top tile1)) :top)
                       (when (t2-edges (:bottom tile1)) :bottom)]))))

(find-edge-matches tile-1951 tile-2311)

(defn edge-matches [test-tile tiles]
  (reduce (fn [A tile]
            (let [matches (find-edge-matches test-tile tile)]
              (if (empty? matches)
                A
                (dissoc (assoc A (:id tile) matches)
                        (:id test-tile)))))
          {}
          tiles))

(edge-matches tile-1951 ex-tiles)

(defn corner? [test-tile tiles]
  (->> (edge-matches test-tile tiles)
       (vals)
       (apply set/union)
       count
       (= 2)))

(corner? tile-1951 ex-tiles)



(apply * (map :id (filter #(corner? % ex-tiles) ex-tiles)))
;; => 20899048083289
(time (apply * (map :id (filter #(corner? % input) input))))
;; => 17250897231301
