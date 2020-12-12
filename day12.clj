(ns day12
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(defn parse-input [input]
  (let [[op v] (split-at 1 input)]
    [(keyword (apply str op)) (Integer/parseInt (apply str v))]))

(def ex (map parse-input (str/split-lines "F10\nN3\nF7\nR90\nF11")))
(def input (map parse-input (str/split-lines (slurp "resources/day12input"))))

(def degrees
  {:N 0 :E 90 :S 180 :W 270})

(defn turn [facing dir deg]
  (let [deg (if (= :R dir) deg (- 360 deg))]
    ((map-invert degrees) (mod (+ deg (facing degrees)) 360))))

(defn move [[x y] face amnt]
  (case face
    :N (map #(vector x (+ y %)) (range 1 (inc amnt)))
    :S (map #(vector x (- y %)) (range 1 (inc amnt)))
    :E (map #(vector (+ x %) y) (range 1 (inc amnt)))
    :W (map #(vector (- x %) y) (range 1 (inc amnt)))))

(defn run-instruction [{:keys [facing track] :as state} [op v]]
  (let [pos (last track)]
    (cond (#{:R :L} op) (assoc state :facing (turn facing op v))
          (= :F op) (update state :track concat (move pos facing v))
          :else (update state :track concat (move pos op v)))))

(let [[x y] (last (:track (reduce run-instruction {:facing :E :track [[0 0]]} ex)))]
  (+ (Math/abs x) (Math/abs y)))

(let [[x y] (last (:track (reduce run-instruction {:facing :E :track [[0 0]]} input)))]
  (+ (Math/abs x) (Math/abs y)))
