(ns day13
  (:require [clojure.string :as str]))

(let [[ts ids] (str/split-lines (slurp "resources/day13input"))
      ts (Long/parseLong ts)
      ids (mapv #(Long/parseLong %) (remove #{"x"} (str/split ids #",")))]
  [ts ids]
  (apply * (first (sort-by second
                           (for [id ids]
                             [id (- (* id (int (Math/ceil (/ ts id)))) ts)])))))
