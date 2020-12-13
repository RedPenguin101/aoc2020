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
    :N (vector x (+ y amnt)) 
    :S (vector x (- y amnt)) 
    :E (vector (+ x amnt) y) 
    :W (vector (- x amnt) y)))

(defn run-instruction [{:keys [facing track] :as state} [op v]]
  (let [pos (last track)]
    (cond (#{:R :L} op) (assoc state :facing (turn facing op v))
          (= :F op) (update state :track conj (move pos facing v))
          :else (update state :track conj (move pos op v)))))

(comment
  (let [[x y] (last (:track (reduce run-instruction {:facing :E :track [[0 0]]} ex)))]
    (+ (Math/abs x) (Math/abs y)))
  ;; => 25

  (let [[x y] (last (:track (reduce run-instruction {:facing :E :track [[0 0]]} input)))]
    (+ (Math/abs x) (Math/abs y)))
  ;; => 415
)

(defn turn-waypoint [[x y] dir deg]
  (let [deg (if (= :R dir) deg (- 360 deg))]
    (case deg
      90  [y (- x)]
      180 [(- x) (- y)]
      270 [(- y) x])))

(defn move-to-waypoint [[x y] [wx wy] units]
  (vector (+ x (* wx units)) (+ y (* wy units))))

(defn run-instruction2 [{:keys [track waypoint] :as state} [op v]]
  (let [pos (last track)]
    (cond (#{:R :L} op) (update state :waypoint turn-waypoint op v)
          (= :F op) (update state :track conj (move-to-waypoint pos waypoint v))
          :else (assoc state :waypoint (move waypoint op v)))))

(comment
  (let [[x y] (last (:track (reduce run-instruction2 {:track [[0 0]] :waypoint [10 1]} input)))]
    (+ (Math/abs x) (Math/abs y))))
;; => 29401
