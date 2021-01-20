(ns aoc2020.day11-2
  (:require [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-input [x-size y-size input-string]
  (set (keep
        (fn [[coord seat]]
          (when (= seat \L) coord))
        (map vector (for [y (range 1 (inc y-size))
                          x (range 1 (inc x-size))]
                      [x y])
             (mapcat vec (str/split-lines input-string))))))

(def ex-input (parse-input 10 10 (slurp "resources/day11example")))

(comment
  "represent the seats as set of coords with 3rd element as set of neighbours"
  #{[1 2 #{[3 4] [0 1]}]}
  "Goal is to reduce redundant calls to neighbours"

  "The only difference for parts 1 and two should be the nested set of 
   'neighbours' for each. For part 1 it is the (up to) 8 seats in the 
   'Moore Neighbourhood', for part 2 it is the (up to) 8 seats visible")

(defn neighbourhood [[x y]]
  (disj (set (for [x' (range (dec x) (+ 2 x))
                   y' (range (dec y) (+ 2 y))]
               [x' y']))
        [x y]))

(defn add-neighbours [all-seats co-ord]
  (conj co-ord (set/intersection all-seats (neighbourhood co-ord))))

(def ex-input-with-n (set (map (partial add-neighbours ex-input) ex-input)))


(defn step
  "Given the set of all seats, and the set of occupied seats, will return
   the set of occupied seats in the next set"
  [all-seats occupieds]
  (tap> (str "seats" (count all-seats)))
  (reduce
   (fn [A seat]
     (let [occ (count (set/intersection (last seat) occupieds))]
       (if
        (or (zero? occ)
            (and (contains? occupieds (take 2 seat))
                 (< occ 5)))
         (conj A (take 2 seat))
         A)))
   #{}
   all-seats))

(defn iterate-to-stability [all-seats occupied-seats it]
  (tap> it)
  (let [new-occupied-seats (step all-seats occupied-seats)]
    (cond (= occupied-seats new-occupied-seats)
          occupied-seats

          (> it 110)
          :break

          :else (recur all-seats new-occupied-seats (inc it)))))

(comment
  (->> #{}
       (step ex-input-with-n)
       (step ex-input-with-n)
       count)

  (count (step ex-input-with-n ex-input-with-n))

  (count (step ex-input-with-n #{}))

  (parse-input 10 10 (slurp "resources/day11example"))

  (mapcat vec (str/split-lines (slurp "resources/day11example")))

  (count (parse-input 10 10 (slurp "resources/day11example")))

  (count (step (parse-input 10 10 (slurp "resources/day11example")) #{})))


(def input (parse-input 97 93 (slurp "resources/day11input")))
(def input-with-neigbours (set (map (partial add-neighbours input) input)))

(comment
  (count (f ex-input-with-n #{} 0))
  (f ex-input-with-n #{} 0)

  (time (count (f input-with-neigbours #{} 0)))

  (step input-with-neigbours input-with-neigbours)

  (count (step (parse-input 93 97 (slurp "resources/day11input")) #{})))

(comment
  "Part 2 starts here"

  "The first seat that can be seen in the 8 directions
   
   So for each seat you get the relative coords and filter on coords that
   are in the form
   [0 a], [a 0], [a, a] [a, -a] (note: would have to distinguish between
   positive and negative a's)
   Then for each of those groups, find the one with the smallest a")

(defn take-closer [current new ref-coord]
  (if (or (nil? current) (> (apply + (map (comp #(Math/abs %) -) current ref-coord))
                            (apply + (map (comp #(Math/abs %) -) new ref-coord))))
    new
    current))

(defn dir [[x y]]
  (cond (and (pos? y) (zero? x)) :up
        (and (neg? y) (zero? x)) :down
        (and (pos? x) (zero? y)) :left
        (and (neg? x) (zero? y)) :right
        (and (pos? x) (= x y)) :up-left
        (and (neg? x) (= x y)) :down-right
        (and (pos? x) (= x (- y))) :down-left
        (and (neg? x) (= x (- y))) :up-right))

(defn find-sightlines [coord others]
  (set (vals (reduce (fn [A other]
                       (if-let [d (dir (map - other coord))]
                         (update A d take-closer other coord)
                         A))
                     {}
                     others))))

(def ex-input-with-sight (map #(conj % (find-sightlines % ex-input)) ex-input))

(comment
  (count (iterate-to-stability ex-input-with-sight #{} 0)))


(first input)

(comment
  (def input (parse-input 97 93 (slurp "resources/day11input")))
  (def input-with-sight (mapv #(conj % (find-sightlines % input)) input))
  (count (iterate-to-stability input-with-sight #{} 0)))
