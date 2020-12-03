(ns day1)

"https://adventofcode.com/2020/day/1"

(first (let [x (map #(Integer/parseInt %) (clojure.string/split (slurp "resources/day1input") #"\n"))]
         (for [a x
               b x
               c x
               :let [d (+ a b c)]
               :when (= d 2020)]
           (* a b c))))


(defn f [xs]
  (let [r (reduce (fn [a b] (if (= (+ a b) 2020) (reduced [a b]) a)) xs)]
    (if (coll? r)
      (apply * r)
      (recur (rest xs)))))

(f [1721 979 366 299 675 1456])
(f [979 1721 366 299 675 1456])

(f (map #(Integer/parseInt %) (clojure.string/split (slurp "resources/day1input") #"\n")))
