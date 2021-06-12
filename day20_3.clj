(ns aoc2020.day20-3
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [ubergraph.core :as uber]))

(defn parse-tiles
  "Returns a dictional of tile-name->tile"
  [s]
  (into {} (let [tiles (map str/split-lines (str/split s #"\n\n"))]
             (for [[nm & tile] tiles]
               [(Long/parseLong (subs nm 5 9)) (mapv #(mapv {\. \. \# \#} %) tile)]))))

(def tile-ref (parse-tiles (slurp "resources/day20example")))
(def example-tiles (vals tile-ref))
(def tile2311 (first example-tiles))
(def tile1951 (second example-tiles))
(def tile1171 (nth example-tiles 2))
(def tile1427 (nth example-tiles 3))
(def tile1489 (nth example-tiles 4))
(def tile2473 (nth example-tiles 5))
(def tile2971 (nth example-tiles 6))
(def tile2729 (nth example-tiles 7))
(def tile3079 (last example-tiles))

(defn flips
  "Given a square tile, will return a set of 8 tiles which are the
   reflective/rotational symmetries of the tile"
  [tile]
  (hash-set tile ;; r0
            (apply mapv vector (reverse tile)) ;; r1
            (mapv reverse (reverse tile)) ;; r2
            (reverse (apply mapv vector tile)) ;;r3
            (reverse tile) ;; v-flip
            (mapv reverse tile) ;; h-flip
            (apply mapv vector tile) ;; flip-diag 1
            (reverse (apply mapv vector (reverse tile))) ;; flip-diag 2
            ))

(defn find-name [tile-ref tile]
  (let [flps (flips tile)]
    (some (fn [[nm ref-tile]]
            (when (flps ref-tile) nm))
          tile-ref)))

(comment
  "all the flips of a tile should return the same name - in this case all 2311"
  (map (partial find-name tile-ref) (flips tile2311)))

(defn match
  "Given two tiles, finds how the two tiles fit together
   Where there is a match, will return a 2-tuple, where the first element is the
   direction tile2 sits in relation to tile1 (above, below, left right)
   and the second element is tile2"
  [tile1 tile2]
  (cond (= (first tile1) (last tile2)) [:above tile2]
        (= (last tile1) (first tile2)) [:below tile2]
        (= (map first tile1) (map last tile2)) [:left tile2]
        (= (map last tile1) (map first tile2)) [:right tile2]
        :else nil))

(defn flip-match [tile1 tile2]
  (some (partial match tile1) (flips tile2)))

(comment
  (match tile1951 tile2729)
  (match tile1951 tile2311)
  (match tile1951 tile3079)
  "These two won't match because tile 3079 needs to be flipped before
   it can be matched"
  (match tile2311 (reverse tile3079))

  "Flip match does the same thing as match, but flips tile2 if needed first"
  (flip-match tile1951 tile2729)
  (flip-match tile1951 tile2311)
  (flip-match tile1951 tile3079)
  "This now works"
  (flip-match tile2311 (reverse tile3079))
  (flip-match tile2311 tile3079)

  (flip-match (second (flip-match tile3079 tile2311))
              tile1951))

(defn print-tile [tile]
  (println (str/join "\n" (map (partial apply str) tile))))

(defn find-match
  "Given a tile and a set of other tiles, will return the first match between
   the tile and one of the other tiles it finds"
  [tile other-tiles]
  (some (partial flip-match tile) other-tiles))

(comment
  (find-match (first example-tiles) (rest example-tiles)))

(defn connections
  ([tiles] (connections '() (first tiles) (set (rest tiles)) []))
  ([path tile other-tiles edges]
   (let [m (find-match tile other-tiles)]
     (cond (empty? other-tiles) edges
           (nil? m) (recur (rest path) (first path) other-tiles edges)
           :else (recur (cons tile path)
                        (second m)
                        (set/difference other-tiles (flips (second m)))
                        (conj edges [tile (second m) (first m)]))))))

(defn connection->name [tile-ref [from to dir]]
  [(find-name tile-ref from)
   (find-name tile-ref to)
   dir])

(comment
  "Tiles 3079, 1951, 2311 and 2473 form a closed digraph, so we can use to test"

  (connections '() tile3079 #{tile1951 tile2311 tile2473} [])

  (map (partial connection->name tile-ref)
       (connections '() tile3079 #{tile1951 tile2311 tile2473} []))

  (def not-tile3079 (disj (set example-tiles) tile3079))
  (count not-tile3079)
  (count example-tiles)

  (connections '() tile3079
               not-tile3079
               [])

  (map (partial connection->name tile-ref)
       (connections '() tile3079 not-tile3079 []))

  (map (partial connection->name tile-ref) (connections example-tiles))
  ;; => ([2729 1951 :below]
  ;;     [1951 2311 :right]
  ;;     [2311 3079 :right]
  ;;     [3079 2473 :above]
  ;;     [2473 1171 :above]
  ;;     [1171 1489 :left]
  ;;     [1489 2971 :left]
  ;;     [1489 1427 :below])

  1)

(defn re-base-coords [m]
  (into {} (let [rebase (map #(apply min %) (apply map vector (vals m)))]
             (map (fn [[k v]] [k (mapv - v rebase)]) m))))

(defn grid-from-coords [m]
  (let [[xmax ymax] (map #(apply max %) (apply map vector (vals m)))
        grid (vec (repeat (inc ymax) (vec (repeat (inc xmax) nil))))]
    (reduce (fn [A [nm coord]]
              (assoc-in A coord nm))
            grid
            m)))

(defn names->grid [cons]
  (grid-from-coords
   (re-base-coords
    (let [origin (ffirst cons)]
      (reduce (fn [A [from to dir]]
                (let [[x y] (get A from)]
                  (assoc A to (case dir
                                :above [x (inc y)]
                                :below [x (dec y)]
                                :left  [(dec x) y]
                                :right [(inc x) y]))))
              {origin [0 0]}
              cons)))))


(comment
  (def names (map (partial connection->name tile-ref) (connections example-tiles)))

  (names->grid names)
  (flips (names->grid names))

  (names->grid (connections example-tiles))

  1)

(defn remove-edges [tile]
  (butlast (rest (map #(butlast (rest %)) tile))))

(defn re-tot [tot]
  (map #(map remove-edges %) tot))

(defn combine [grid-of-tiles]
  (str/join "\n" (mapcat  #(map str/join (apply mapv concat %)) grid-of-tiles)))

(comment
  (def x [[[[:a1 :a2 :a3]
            [:a4 :a5 :a6]
            [:a7 :a8 :a9]]
           [[:b1 :b2 :b3]
            [:b4 :b5 :b6]
            [:b7 :b8 :b9]]]
          [[[:c1 :c2 :c3]
            [:c4 :c5 :c6]
            [:c7 :c8 :c9]]
           [[:d1 :d2 :d3]
            [:d4 :d5 :d6]
            [:d7 :d8 :d9]]]])

  (remove-edges [[1 2 3 :x]
                 [4 5 6 :y]
                 [7 8 9 :z]
                 [:a :b :c :d]])

  (re-tot x)

  (print (combine (re-tot (names->grid (connections example-tiles)))))

  1)
