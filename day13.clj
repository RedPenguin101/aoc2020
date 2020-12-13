(ns day13
  (:require [clojure.string :as str]
            [criterium.core :refer [quick-bench with-progress-reporting]]))

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

(comment
  (linear-congruence-solve [[0 7] [-1 13] [-4 59] [-6 31] [-7 19]])
  ;; => 1068781N


  (linear-congruence-solve [[0 17] [-2 13] [-3 19]])
  ;; => 3417N
  (linear-congruence-solve [[0 67] [-1 7] [-2 59] [-3 61]])
  ;; => 754018N
  (linear-congruence-solve [[0 67] [-2 7] [-3 59] [-4 61]])
  ;; => 779210N
  (linear-congruence-solve [[0 67] [-1 7] [-3 59] [-4 61]])
  ;; => 1261476N
  (linear-congruence-solve [[0 1789] [-1 37] [-2 47] [-3 1889]])
  ;; => 1202161486N
  (linear-congruence-solve input)
  ;; => 538703333547789N

  (with-progress-reporting (quick-bench (linear-congruence-solve input) :verbose)))
