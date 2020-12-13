(ns day13
  (:require [clojure.string :as str]))

(let [[ts ids] (str/split-lines (slurp "resources/day13input"))
      ts (Long/parseLong ts)
      ids (mapv #(Long/parseLong %) (remove #{"x"} (str/split ids #",")))]
  [ts ids]
  (apply * (first (sort-by second
                           (for [id ids]
                             [id (- (* id (int (Math/ceil (/ ts id)))) ts)])))))

(defn- linear-congruence-solve [pairs]
  (let [remainders (map first pairs)
        modulos (map second pairs)
        prod (apply * modulos)
        pp (map #(/ prod %) modulos)
        inv (map #(.modInverse (biginteger %1) (biginteger %2)) pp modulos)]
    (mod (apply + (map * remainders pp inv)) prod)))

(def input (keep-indexed
            (fn [i n] (when n
                        [(bigint (- i)) n]))
            (map #(try (bigint (Integer/parseInt %))
                       (catch Exception e nil))
                 (str/split (second (str/split-lines (slurp "resources/day13input"))) #","))))

(linear-congruence-solve input)
