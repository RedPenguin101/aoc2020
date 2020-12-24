(ns day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse-tile [tile-block]
  (let [[id-row & tile] (str/split-lines tile-block)
        tile (mapv vec tile)]
    {:id (Integer/parseInt (subs id-row 5 9))
     :borders (let [flipped (apply map vector tile)] (vector (first tile) (last flipped) (last tile) (first flipped)))
     :tile tile}))

(def ex-tiles (map parse-tile (str/split (slurp "resources/day20example") #"\n\n")))
(def input (map parse-tile (str/split (slurp "resources/day20input") #"\n\n")))

(comment
  "A tile looks like this:"
  {:id 2311
   :borders
   [[\. \. \# \# \. \# \. \. \# \.]
    [\. \. \. \# \. \# \# \. \. \#]
    [\. \. \# \# \# \. \. \# \# \#]
    [\. \# \# \# \# \# \. \. \# \.]]
   :tile
   [[\. \. \# \# \. \# \. \. \# \.]
    [\# \# \. \. \# \. \. \. \. \.]
    [\# \. \. \. \# \# \. \. \# \.]
    [\# \# \# \# \. \# \. \. \. \#]
    [\# \# \. \# \# \. \# \# \# \.]
    [\# \# \. \. \. \# \. \# \# \#]
    [\. \# \. \# \. \# \. \. \# \#]
    [\. \. \# \. \. \. \. \# \. \.]
    [\# \# \# \. \. \. \# \. \# \.]
    [\. \. \# \# \# \. \. \# \# \#]]})

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

(comment
  "the border matches of a tile are all the tiles it is connected to
   2 matches denotes a corner, 3 an edge
   These are given as a map of id->[matches]"

  (border-matches ex-tiles)
  {2729 '(2971 1427 1951 nil)
   1171 '(2473 1489 nil nil)
   2971 '(nil 1489 2729 nil)
   2311 '(1427 3079 nil 1951)
   1489 '(nil 1171 1427 2971)
   1427 '(1489 2473 2311 2729)
   3079 '(nil nil 2473 2311)
   2473 '(nil 3079 1427 1171)
   1951 '(2729 2311 nil nil)})

(defn- corners [tiles]
  (->> (border-matches tiles)
       (filter #(= 2 (count (remove nil? (second %)))))
       keys))

(apply * (corners ex-tiles))
(time (apply * (corners input)))
;; => 17250897231301

(defn- xforms [[a b c d]]
  (vector [[a b c d] :r0]
          [[d a b c] :r1]
          [[c d a b] :r2]
          [[b c d a] :r3]
          [[c b a d] :fv]
          [[a d c b] :fh]
          [[b a d c] :fd1]
          [[d c b a] :fd2]))

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

(defn- find-fit* [a [id2 conns2]]
  (first (filter #(fit? a %) (map (partial apply vector id2) (xforms conns2)))))

(defn- find-fit
  "where a is the left, and b is below"
  [a b [id2 conns2]]
  (cond (nil? a) (find-fit* b [id2 conns2])
        (nil? b) (find-fit* a [id2 conns2])
        :else
        (first (set/intersection (set (filter #(fit? b %) (map (partial apply vector id2) (xforms conns2))))
                                 (set (filter #(fit? a %) (map (partial apply vector id2) (xforms conns2))))))))

(def input-border-matches (border-matches input))
(def ex-tiles-matches (border-matches ex-tiles))

(defn pos-manhatten-distance [n]
  (for [x (range (inc n))
        y (range (inc n))
        :when (= (+ x y) n)]
    [x y]))

(defn build-grid [tiles state man-dist]
  (into {}
        (remove #(nil? (second %))
                (reduce (fn [A [x y]]
                          (let [down (A [x (dec y)])
                                left (A [(dec x) y])
                                this-id  (or (second (second left)) (first (second down)))]
                            (assoc A [x y] (find-fit down left [this-id (tiles this-id)]))))
                        state
                        (rest (mapcat pos-manhatten-distance (range man-dist)))))))

(comment
  "build grid takes tiles, your 'starter' and the max distance to test, and returns
   a mapping of coordinates on a 2d plane to a tuple of the id of the tile, the connections to
   other tiles (top, right, left, bottom) and the xforms the tile is required to go through."

  (build-grid ex-tiles-matches {[0 0] [2971 [2729 1489 nil nil] :fv]} 5)
  {[2 2] [3079 [nil nil 2473 2311] :r0]
   [0 0] [2971 [2729 1489 nil nil] :fv]
   [1 0] [1489 [1427 1171 nil 2971] :fv]
   [1 1] [1427 [2311 2473 1489 2729] :fv]
   [0 2] [1951 [nil 2311 2729 nil] :fv]
   [2 0] [1171 [2473 nil nil 1489] :fh]
   [2 1] [2473 [3079 nil 1171 1427] :fd1]
   [1 2] [2311 [nil 3079 1427 1951] :fv]
   [0 1] [2729 [1951 1427 2971 nil] :fv]})

(comment
  "this is a helper function to check the grid connects up properly"
  (defn check-grid [grid]
    (for [[[x y] [id [up right down left]]] grid]
      (and (= up (first (grid [x (inc y)])))
           (= right (first (grid [(inc x) y])))
           (= down (first (grid [x (dec y)])))
           (= left (first (grid [(dec x) y]))))))

  (check-grid (build-grid ex-tiles-matches {[0 0] [2971 [2729 1489 nil nil] :fv]} 5))
  (every? true? (check-grid (build-grid input-border-matches {[0 0] [1321 [3761 2293 nil nil] :none]} 24))))

(defn flip [tile dir]
  (case dir
    :fv (reverse tile)
    :fh (mapv reverse tile)
    :fd1 (apply mapv vector tile)
    :fd2 (flip (flip tile :r1) :fv)
    :r0 tile
    :r1 (apply mapv vector (reverse tile))
    :r2 (flip (flip tile :r1) :r1)
    :r3 (flip (flip tile :fh) :fd1)))

(defn trim-tile [tile]
  (map (comp rest butlast) (rest (butlast tile))))

(comment
  (trim-tile [[:a :b :c :x]
              [:d :e :f :y]
              [:g :h :i :z]
              [:v :w :q :r]]))

(defn build-grid2 [grid-defn tiles size]
  (partition size (for [y (reverse (range size))
                        x (range size)]
                    (let [[id _ xf] (get grid-defn [x y])]
                      (str/join "\n" (map str/join (flip (trim-tile (:tile (first (filter #(= id (:id %)) tiles)))) xf)))))))

(println (build-grid2 (build-grid ex-tiles-matches {[0 0] [2971 [2729 1489 nil nil] :fv]} 5) ex-tiles 3))
(build-grid ex-tiles-matches {[0 0] [2971 [2729 1489 nil nil] :fv]} 5)
(group-by :id ex-tiles)