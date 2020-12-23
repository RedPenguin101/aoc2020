(ns day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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
  (vector [[a b c d] :none]
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

(defn- find-fit2
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

(defn f [tiles state man-dist] 
  (into {}
        (remove #(nil? (second %))
                (reduce (fn [A [x y]]
                          (let [down (A [x (dec y)])
                                left (A [(dec x) y])
                                this-id  (or (second (second left)) (first (second down)))]
                            (assoc A [x y] (find-fit2 down left [this-id (tiles this-id)]))))
                        state
                        (rest (mapcat pos-manhatten-distance (range man-dist)))))))

(comment
  (find-fit2 [2729 [1427 1951 nil 2971] :r3] nil [1951 [2729 2311 nil nil]])
  (find-fit2 (find-fit* [2971 [2729 1489 nil nil] :fv] [2729 ((border-matches ex-tiles) 2729)])
             (find-fit* [2971 [2729 1489 nil nil] :fv] [1489 ((border-matches ex-tiles) 1489)])
             [1427 ((border-matches ex-tiles) 1427)])

  (f ex-tiles-matches {[0 0] [2971 [2729 1489 nil nil] :fv]} 5)
  ;; => {[2 2] [3079 [nil nil 2473 2311] :none],
  ;;     [0 0] [2971 [2729 1489 nil nil] :fv],
  ;;     [1 0] [1489 [1427 1171 nil 2971] :fv],
  ;;     [1 1] [1427 [2311 2473 1489 2729] :fv],
  ;;     [0 2] [1951 [nil 2311 2729 nil] :fv],
  ;;     [2 0] [1171 [2473 nil nil 1489] :fh],
  ;;     [2 1] [2473 [3079 nil 1171 1427] :fd1],
  ;;     [1 2] [2311 [nil 3079 1427 1951] :fv],
  ;;     [0 1] [2729 [1951 1427 2971 nil] :fv]}

  (f input-border-matches {[0 0] [1321 [3761 2293 nil nil] :none]} 24)
  (count (f input-border-matches {[0 0] [1321 [3761 2293 nil nil] :none]} 24))
  ((f input-border-matches {[0 0] [1321 [3761 2293 nil nil] :none]} 24) [12 12]))

(defn check-grid [grid]
  (for [[[x y] [id [up right down left]]] grid]
    (and (= up (first (grid [x (inc y)])))
         (= right (first (grid [(inc x) y])))
         (= down (first (grid [x (dec y)])))
         (= left (first (grid [(dec x) y]))))))

(check-grid (f ex-tiles-matches {[0 0] [2971 [2729 1489 nil nil] :fv]} 5))
(every? true? (check-grid (f input-border-matches {[0 0] [1321 [3761 2293 nil nil] :none]} 24)))


(def tile [[:a :b :c]
           [:d :e :f]
           [:g :h :i]])

(defn trim-tile [tile]
  (mapv #(rest %) (mapv #(drop 1 %) (vec (rest (drop 1 tile))))))

(trim-tile tile)
(:tile (first input))
(trim-tile (:tile (first input)))
