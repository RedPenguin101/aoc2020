(ns aoc2020.day24
  "This namespace deals with hexagonal co-ordinate systems.

   There are two representations of hexagonal coordinates used here. The first
   is a string of relative directions, eg. 'eswnwewne'. This is used exclusively
   for input/serialization

   The second system is a 3dimensional coordinate system. A hex is represented
   as [x y z]. This is used for actual operations"

  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse-directions
  "Given an input string in the form of non-delimited directions,
   e.g. 'ewswnwse' returns a vector of directions"
  [direction-string]
  (reduce
   (fn [A letter]
     (if (#{"s" "n"} (last A))
       (conj (vec (butlast A)) (str (last A) letter))
       (conj A (str letter))))
   []
   direction-string))

(defn- hex-coord-shift
  "Given a hex coordinate [x y z] and a direction e w sw se nw ne, will return
   the coordinates of the adjcant hex in that direction"
  [[x y z] dir]
  (case dir
    "e"  [(inc x) (dec y)      z]
    "ne" [(inc x)      y  (dec z)]
    "nw" [x  (inc y) (dec z)]
    "se" [x  (dec y) (inc z)]
    "w"  [(dec x) (inc y)      z]
    "sw" [(dec x)      y  (inc z)]))

(defn hex
  "given a direction string in the form 'wneswe...' etc., will return the coords
   of the hex resulting from 'walking' those directions from a starting hex
   (if provided) or from origin hex [0 0 0] (if not)"

  ([direction-string] (hex [0 0 0] direction-string))

  ([start direction-string]
   (reduce hex-coord-shift start
           (parse-directions direction-string))))

(comment
  "Part 1"

  (->> "resources/day24example"
       slurp
       (str/split-lines)
       (map hex)
       frequencies
       (filter #(odd? (second %)))
       count)

  (->> "resources/day24input"
       slurp
       (str/split-lines)
       (map hex)
       frequencies
       (filter #(odd? (second %)))
       count))

(defn initial-black-tiles
  "Given a list of direction strings, returns a list of hexes
   corresponding to only the black tiles. i.e. those that are flipped an odd
   number of times"
  [dir-strs]
  (set (reduce (fn [A [coord freq]]
                 (if (odd? freq)
                   (conj A coord)
                   A))
               [] (frequencies (map hex dir-strs)))))

(defn adjacents
  "Given a hex, returns a set of all hexes adjacent to it."
  [[x y z]]
  (set [[(inc x) (dec y)      z]
        [(inc x)      y  (dec z)]
        [x  (inc y) (dec z)]
        [x  (dec y) (inc z)]
        [(dec x) (inc y)      z]
        [(dec x)      y  (inc z)]]))

(defn black?
  "Given a set of currently black tiles and a tile, will return true if the tile
   will be black in the subsequent 'step'"
  [black-tiles tile]
  (let [adjacent-blacks (count (set/intersection black-tiles (adjacents tile)))
        tile-is-black?  (contains? black-tiles tile)]

    (if tile-is-black?
      (<= 1 adjacent-blacks 2)
      (= 2 adjacent-blacks))))

(defn all-adjacents [tiles]
  (set (mapcat adjacents tiles)))

(defn step [current-black-tiles]
  (set (filter (partial black? current-black-tiles)
               (set/union current-black-tiles (all-adjacents current-black-tiles)))))

(defn sim-days
  ([n tiles]
   (sim-days n tiles 0))
  ([n tiles i]
   (if (= n i)
     tiles
     (recur n (step tiles) (inc i)))))

(comment
  (def example-init-state (initial-black-tiles (->> "resources/day24example"
                                                    slurp
                                                    (str/split-lines))))

  (count (sim-days 100 example-init-state))

  (def init-state (initial-black-tiles (->> "resources/day24input"
                                            slurp
                                            (str/split-lines))))

  (time (count (sim-days 100 init-state))))
