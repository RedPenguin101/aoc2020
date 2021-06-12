(ns aoc2020.day23)

(def input [7 1 2 6 4 3 5 8 9])

(def example [3 8 9 1 2 5 4 6 7])

(defn destination [current rst]
  (let [r (sort rst)]
    (if (< current (first r))
      (last r)
      (last (filter #(> current %) r)))))

(defn index-of [tgt xs]
  (first (keep-indexed (fn [idx v] (when (= tgt v) idx)) xs)))

(index-of 10 [2 5 4 6 7])

(destination 3 [2 5 4 6 7])
(destination 2 [5 4 6 7 3])
(destination 5 [8 9 1 3 2])

(sort [2 5 4 6 7])

(defn shift [coll]
  (conj (vec (rest coll)) (first coll)))

(defn shift-to-one [coll]
  (if (= 1 (first coll))
    coll
    (recur (shift coll))))

(shift '(3 2 8 9 1 5 4 6 7))

(defn f [[current pu1 pu2 pu3 & rst]]
  (let [[l r] (split-at (inc (index-of (destination current rst) rst)) rst)]
    (shift (flatten [current l pu1 pu2 pu3 r]))))

(defn g [xs n]
  (if (zero? n)
    xs
    (recur (f xs) (dec n))))

(g example 100)
(clojure.string/join (rest (shift-to-one (g input 100))))
;; => "29385746"
