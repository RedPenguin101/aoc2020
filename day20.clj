(ns day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse-tile [tile-block]
  (let [[id-row & tile] (str/split-lines tile-block)
        tile (mapv vec tile)]
    {:id (Integer/parseInt (subs id-row 5 9))
     :borders (let [flipped (apply map vector tile)]
                (vector (first tile) (last flipped) (last tile) (first flipped)))
     :tile tile}))


(defn- find-match [border tiles]
  (remove nil? (for [tile tiles]
                 (when ((set (concat (map reverse (:borders tile)) (:borders tile))) border)
                   (:id tile)))))

(defn- border-matches [tiles]
  (reduce
   (fn [A tile]
     (assoc A (:id tile)

            (for [border (:borders tile)]
              (first (remove #(= % (:id tile)) (find-match border tiles))))))
   {}
   tiles))

(defn- corners [tiles]
  (->> (border-matches tiles)
       (filter #(= 2 (count (remove nil? (second %)))))
       keys))

(def ex-tiles (map parse-tile (str/split (slurp "resources/day20example") #"\n\n")))
(def input (map parse-tile (str/split (slurp "resources/day20input") #"\n\n")))

(apply * (corners ex-tiles))
(time (apply * (corners input)))
;; => 17250897231301
