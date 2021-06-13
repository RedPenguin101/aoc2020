(ns day20-4
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert] :as set]))

"Day 20: Jurassic Jigsaw
 
 square image tiles that need to be reassembled back into a single image.
 ID number
 each image tile has been rotated and flipped to a random orientation
 orient the tiles so they fit together
 each tile's image data includes a border that should line up 
 exactly with its adjacent tiles
 outermost edges won't line up with any other tiles.
 What do you get if you multiply together the IDs of the four corner tiles?"

(defn parse [m block]
  (let [[id & tile] (str/split block #"\n")]
    (assoc m (Long/parseLong (re-find #"\d+" id)) tile)))

(def input (reduce parse {} (str/split (slurp "resources/day20example") #"\n\n")))

(defn edges [tile]
  {:top (first tile)
   :bottom (last tile)
   :left (apply str (map first tile))
   :right (apply str (map last tile))})

(defn permutations [tile]
  (let [inner         (comp (partial apply str) vector)
        inner-reverse (comp (partial apply str) reverse)]
    [tile ;; identity
     (reverse tile) ;; flip horizontal 

     (map inner-reverse tile) ;; flip vertical
     (map inner-reverse (reverse tile)) ;; rotate 180 (fh+fv)

     (apply map inner tile) ;; flip d2
     (apply map inner (reverse tile)) ;; rotate 90 (fh+fd2)

     (reverse (apply map inner tile)) ;; rotate 270
     (reverse (apply map inner (reverse tile))) ;; flip d1
     ]))

(comment (permutations ["123" "456" "789"]))

(defn find-tile-id
  "Given a map of id->tile, and a tile in any permutation, will find the
  id of that tile"
  [m tile]
  (some #((map-invert m) %) (permutations tile)))

(comment
  (let [tiles (permutations (input 2473))]
    (map #(find-tile-id input %) tiles)))

"simple graph for connections"

(def opposing {:left-of :right-of
               :right-of :left-of
               :above :below
               :below :above})

(defn add-edge [edges [tile1 direction tile2 :as relation]]
  (if relation
    (conj edges [tile1 direction tile2] [tile2 (opposing direction) tile1])
    edges))

(defn same-tile? [tile1 tile2]
  (= (set (permutations tile1)) (set (permutations tile2))))

(comment
  (same-tile? ["123" "456" "789"]
              ["789" "456" "123"])
  (same-tile? ["123" "456" "789"]
              ["789" "X56" "123"]))

(defn match [tile1 tile2]
  (let [tile1-edges (edges tile1)
        tile2-edges (edges tile2)]
    (cond (same-tile? tile1 tile2) nil
          (= (:top tile1-edges) (:bottom tile2-edges))
          [tile1 :above tile2]
          (= (:bottom tile1-edges) (:top tile2-edges))
          [tile1 :below tile2]
          (= (:left tile1-edges) (:right tile2-edges))
          [tile2 :left-of tile1]
          (= (:right tile1-edges) (:left tile2-edges))
          [tile2 :right-of tile1]
          :else nil)))

(defn find-match [anchor-tile other-tile]
  (some #(match anchor-tile %) (permutations other-tile)))

(comment
  (find-match (input 1951) (input 2311))
  (find-match (input 1951) (input 3079)))

(defn find-edges
  ([tile other-tiles] (find-edges tile other-tiles []))
  ([tile other-tiles edges]
   (cond (empty? other-tiles) edges
         :else (recur tile
                      (rest other-tiles)
                      (add-edge edges (find-match tile (first other-tiles)))))))

(defn edge->id [tile-map [from dir to]]
  [(find-tile-id tile-map from)
   dir
   (find-tile-id tile-map to)])

(comment
  (map #(edge->id input %) (find-edges (input 1951) [(input 2311) (input 2729) (input 3079)]))

  (let [[anchor & ids] (keys input)]
    (map #(edge->id input %) (find-edges (input anchor) (map input ids)))))

(defn build-graph
  ([tiles] (build-graph (find-edges (first tiles) (rest tiles)) tiles))
  ([edges tiles]
   (if (empty? tiles) (do (println "finished") edges)
       (let [processed-tiles (set (map first edges))
             new-matches (keep #(find-match % (first tiles)) processed-tiles)]
         (if (empty? new-matches)
           (recur edges (conj (vec (rest tiles)) (first tiles)))
           (recur (set (reduce add-edge edges new-matches))
                  (rest tiles)))))))

(defn corners [full-graph]
  (keep (fn [[v frq]]
          (when (= 2 frq) v))
        (frequencies (map first full-graph))))

(comment
  (let [tiles [(input 1951) (input 2729) (input 2311) (input 3079) (input 1427)]]
    (sort (map #(edge->id input %) (build-graph tiles))))

  (sort (map #(edge->id input %) (build-graph (vals input))))
  (count (set (map first (sort (map #(edge->id input %) (build-graph (vals input)))))))

  (map #(find-tile-id input %) (vals input))

  (let [edges (sort (map #(edge->id input %) (build-graph (vals input))))]
    (frequencies (map first edges)))

  (map #(find-tile-id input %) (corners (build-graph (vals input)))))

(comment
  "Part 1"
  (def real-input (reduce parse {} (str/split (slurp "resources/day20input") #"\n\n")))

  (time (def real-graph (build-graph (vals real-input))))
  ;; 100 seconds, bleh

  (sort (map #(edge->id real-input %) real-graph))

  (->> real-graph
       corners
       (map #(find-tile-id real-input %))
       (apply *))

  (filter #(corners (first %)) (sort (map #(edge->id real-input %) real-graph))))
  ;; bottom left:  1321
  ;; bottom right: 2099
  ;; top left:     2879
  ;; top right:    2161

(defn find-relation [edges tile direction]
  (some (fn [[from dir to]]
          (when (and (= to tile) (= dir direction))
            from))
        edges))

(comment
  (map #(find-tile-id real-input %) (corners real-graph))
  ;; => (2161 2099 1321 2879)
  ;;     TR   BR   BL   TL

  (def bottom-left (nth (corners real-graph) 2))
  (find-tile-id real-input bottom-left)
  ;; => 1321

  bottom-left
  (find-relation real-graph bottom-left :above))

(defn walk [graph tiles]
  (let [above (find-relation graph (last tiles) :above)]
    (if above (recur graph (conj tiles above))
        (let [right (find-relation graph (first tiles) :right-of)]
          (if right
            (concat [tiles] (walk graph [right]))
            [tiles])))))

(comment
  (find-relation real-graph bottom-left :above)
  (def grid (walk real-graph [bottom-left]))
  (count grid)
  ;; => 12
  (map count grid)
  ;; => (12 12 12 12 12 12 12 12 12 12 12 12)

  1)

(defn remove-border [tile]
  (rest (butlast (map #(apply str (rest (butlast %))) tile))))

(defn stitch-column [col]
  (reverse (apply concat (map remove-border col))))

(defn stich-rows [rows] ;; row = 96 length vector of 8 length strings 
  (apply map (fn [& strings]
               #_(println strings)
               (apply str strings))
         rows))



(comment
  "example part 2"
  (def example-graph (build-graph (vals input)))
  (map #(edge->id input %) example-graph)

  (map #(find-tile-id input %) (corners example-graph))
  ;; => (2971 1171 1951 3079)
  ;; =>  BL   BR    TL   TR

  (find-relation example-graph (nth (corners example-graph) 3) :right-of)

  (def example-bl (first (corners example-graph)))

  (def ex-grid (walk example-graph [example-bl]))
  (count (stich-rows (map stitch-column (walk example-graph [example-bl]))))
  ;; => 24

  (map count (stich-rows (map stitch-column (walk example-graph [example-bl]))))
  ;; => (24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24)

  (reverse (first ex-grid))
  (map #(find-tile-id input %) (reverse (first ex-grid)))

  (def example-canvas (stich-rows (map stitch-column ex-grid)))

  (permutations (stich-rows (map stitch-column ex-grid)))

  1)

(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(count (first monster))

(def monster-spotter
  [#".{18}#."
   #"#.{4}##.{4}##.{4}###"
   #".#..#..#..#..#..#..."])

(defn spot-monster [strings]
  (every? seq (map #(re-matches %1 %2) monster-spotter strings)))

(comment
  (def flipped (nth (permutations example-canvas) 4))

  (map-indexed (fn [i s] (when (re-find #".{18}#." s) i)) flipped)
  (map #(re-find #"#.{4}##.{4}##.{4}###" %) flipped)
  (map #(re-find #".#..#..#..#..#..#..." %) flipped)

  (remove nil? (map-indexed (fn [i s] (when (re-find #".{18}#." s) i)) flipped))
  (remove nil? (map-indexed (fn [i s] (when (re-find #"#.{4}##.{4}##.{4}###" s) i)) flipped))
  (remove nil? (map-indexed (fn [i s] (when (re-find #".#..#..#..#..#..#..." s) i)) flipped)))

(defn count-monsters [canvas]
  (count (remove false?
                 (for [block (partition 3 1 canvas)
                       i (range (- (count (first block)) 20))]
                   (spot-monster (map #(subs % i (+ 20 i)) block))))))

(defn count-hashes [canvas] (frequencies (apply concat canvas)))

(comment
  (map #(count-monsters %) (permutations example-canvas))

  (- ((count-hashes flipped) \#) (* 15 (count-monsters flipped)))
  ;; => 273
  )

(comment
  "real part 2"
  bottom-left
  (remove-border bottom-left)

  (count (stitch-column (first grid)))
;; => 96
  (map count (stitch-column (first grid)))
;; => (8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 ...)

  (def real-grid (stich-rows (map stitch-column grid)))

  (map count-monsters (permutations real-grid))
  ;; => (0 0 0 23 0 0 0 0)

  (count-hashes real-grid)
  ;; => {\. 7295, \# 1921}

  (- 1921 (* 23 15))
  ;; => 1576
  )