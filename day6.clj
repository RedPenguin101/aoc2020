(ns day6
  (:require [clojure.string :as str]))

(defn count-answers [input]
  (->> (str/split input #"\R\R")
       (map (comp count set #(str/replace % #"\n" "")))
       (apply +)))

(count-answers (slurp "resources/day6input"))

(defn number-of-all-yes [response]
  (->> response
       (map frequencies)
       (apply merge-with +)
       (filter (fn [[_ v]] (= v (count response))))
       (map first)
       count))

(defn count-all-yes-answers [input]
  (->> (str/split input #"\R\R")
       (map #(str/split % #"\n"))
       (map number-of-all-yes)
       (apply +)))

(count-all-yes-answers (slurp "resources/day6input"))
