(ns day20
  (:require [clojure.string :as str]))

(defn- parse-tile [tile-block]
  (let [[id-row & tile] (str/split-lines tile-block)
        tile (mapv vec tile)]
    {:id (Integer/parseInt (subs id-row 5 9))
     :borders (let [flipped (apply map vector tile)] (vector (first tile) (last flipped) (last tile) (first flipped)))
     :tile tile}))

(defn- find-match [tiles id border]
  (some (fn [tile]
          (let [connections (set (concat (map reverse (:borders tile)) (:borders tile)))]
            (when (and (not= id (:id tile)) (connections border)) (:id tile))))
        tiles))

(defn- border-matches [tiles]
  (reduce
   (fn [A {:keys [id borders]}]
     (assoc A id (map (partial find-match tiles id) borders)))
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

(defn- xforms [[a b c d]]
  (vector [a b c d] [d a b c] [c d a b] [b c d a] 
          [c b a d] [a d c b]
          [b a d c]))

(defn- position-of [x ys]
  (first (keep-indexed (fn [idx val] (when (= x val) idx)) ys)))

(defn- fit?
  "Returns true if the first tile fits with the second without transformation"
  [[id1 conns1] [id2 conns2]]
  (and (= ({0 2 1 3 2 0 3 1} (position-of id1 conns2)) (position-of id2 conns1))
       (if (some #(= 2 (count (filter nil? %))) [conns1 conns2])
         (some (partial apply =) (map vector conns1 conns2))
         true)
       (if (every? #(= 1 (count (filter nil? %))) [conns1 conns2])
         (some (partial apply =) (map vector conns1 conns2))
         true)))

(defn- find-fit [a [id2 conns2]]
  (some #(when (fit? a %) %)
        (map #(vector id2 %) (xforms conns2))))

(defn- follow [direction this ys]
  (cons this
        (lazy-seq
         (follow direction
                 (let [next-id (direction (second this))]
                   (find-fit this [next-id (ys next-id)]))
                 ys))))

(defn get-picture [start tiles]
  (let [ys (into (sorted-map) (border-matches tiles))]
    (reverse (for [row-header (take-while seq (follow first start ys))]
               (take-while seq (follow second row-header ys))))))

(def input-border-matches (border-matches input))

(map #(map first %) (get-picture [1321 '(3761 2293 nil nil)] input))
