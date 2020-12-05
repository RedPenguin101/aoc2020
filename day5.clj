(ns day5
  (:require [clojure.string :refer [split-lines]]
            [clojure.set :refer [difference]]))

(letfn [(f [v char]
          (fn [string] (->> string
                            (map vector v)
                            (filter (comp #{char} second))
                            (map first)
                            (apply +))))]
  (def row (f [64 32 16 8 4 2 1] \B))
  (def col (f [4 2 1] \R)))

(defn seat-id [string]
  (+ (* 8 (row (subs string 0 7)))
     (col (subs string 7))))

(comment
  (seat-id "FBFBBFFRLR")
  (seat-id "BFFFBBFRRR")
  (seat-id "FFFBBBFRRR")
  (seat-id "BBFFBBFRLL")

  (apply max (for [boarding-pass (clojure.string/split-lines (slurp "resources/day5input"))]
               (seat-id boarding-pass)))
  ;; => 885

  (let [taken-seats (sort (map seat-id (split-lines (slurp "resources/day5input"))))]
    (difference (set (range (first taken-seats) (last taken-seats)))
                (set taken-seats)))
  ;; => #{623}
  )