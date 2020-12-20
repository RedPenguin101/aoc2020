(ns day20
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

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

(sort-by #(count (remove nil? (second %))) (border-matches ex-tiles))
;; => ([1171 (2473 1489 nil nil)]
;;     [2971 (nil 1489 2729 nil)]
;;     [3079 (nil nil 2473 2311)]
;;     [1951 (2729 2311 nil nil)]
;;     [2729 (2971 1427 1951 nil)]
;;     [2311 (1427 3079 nil 1951)]
;;     [1489 (nil 1171 1427 2971)]
;;     [2473 (nil 3079 1427 1171)]
;;     [1427 (1489 2473 2311 2729)])

(defn- xforms [[a b c d]]
  (vector [a b c d] [d a b c] [c d a b] [b c d a] 
          [c b a d] [a d c b]
          [b a d c]))

(combo/permutations [:a :b :c :d])

(def x {0 2 1 3 2 0 3 1})

(defn- position-of [x ys]
  (first (keep-indexed (fn [idx val] (when (= x val) idx)) ys)))

(defn- fit?
  "Returns true if the first tile fits with the second without transformation"
  [[id1 conns1] [id2 conns2]]
  (and (= (x (position-of id1 conns2)) (position-of id2 conns1))
       (if (some #(= 2 (count (filter nil? %))) [conns1 conns2])
         (some (partial apply =) (map vector conns1 conns2))
         true)
       (if (every? #(= 1 (count (filter nil? %))) [conns1 conns2])
         (some (partial apply =) (map vector conns1 conns2))
         true)))

(fit? [2381 [2837 1543 3761 nil]] [2837 [2549 nil 2381 2903]])
;; shoul be false
(fit? [2381 [2837 1543 3761 nil]] [2837 [2549 2903 2381 nil]])

(defn- find-fit [a [id2 conns2]]
  (some #(when (fit? a %) %)
        (map #(vector id2 %) (xforms conns2))))


;; [2381 [2837 1543 3761 nil]]
;; => ([2837 [2549 nil 2381 2903]] [2837 [2549 2903 2381 nil]])


(defn- bottom-left? [[id [a b c d]]]
  (and (not (nil? a))
       (not (nil? b))
       (nil? c)
       (nil? d)))

(defn- follow [direction this ys]
  (cons this
        (lazy-seq
         (follow direction
                 (let [next-id (direction (second this))]
                   (find-fit this [next-id (ys next-id)]))
                 ys))))

(into (sorted-map) (border-matches ex-tiles))

(defn get-picture [start tiles]
  (let [ys (into (sorted-map) (border-matches tiles))]
    (reverse (for [row-header (take-while seq (follow first start ys))]
               (take-while seq (follow second row-header ys))))))


(filter bottom-left? (border-matches input))

(def input-border-matches (border-matches input))
;; => (1787 2939 1321 nil)

(take-while seq (follow second [1321 '(3761 2293 nil nil)] input-border-matches))


(corners input)

(get-picture [2971 '(2729 1489 nil nil)] ex-tiles)


(input-border-matches 2837)
(input-border-matches 2381)

(find-fit [2381 [2837 1543 3761 nil]] [2837 '(2549 nil 2381 2903)])

(get-picture [1321 '(3761 2293 nil nil)] input)
(map #(map first %) (get-picture [1321 '(3761 2293 nil nil)] input))

