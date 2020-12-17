(ns day17
  (:require [clojure.string :as str]))

(defn- parse-input [input]
  (remove nil? (apply concat (map-indexed (fn bleh [idx row] (map-indexed #(if (= \# %2) [idx %1 0] nil) row)) (str/split-lines input)))))

(defn- neighbors [[x y z]]
  (for [xvar [-1 0 1]
        yvar [-1 0 1]
        zvar [-1 0 1]
        :when  (not= xvar yvar zvar 0)]
    [(+ x xvar) (+ y yvar) (+ z zvar)]))

(defn- step [coords]
  (let [stuff (reduce (fn [A [c freq]] (if (#{2 3} freq) (update A freq conj c) A)) {} (frequencies (apply concat (map neighbors coords))))]
    (concat (get stuff 3)
            (filter (set coords) (get stuff 2)))))

(defn- print-layer [[z coords] [xmin xmax ymin ymax]]
  (str "z=" z "\n"
       (let [coords (set (map #(take 2 %) coords))]
         (str/join "\n"
                   (map str/join
                        (partition (- (inc xmax) xmin)
                                   (for [x (range xmin (inc xmax))
                                         y (range ymin (inc ymax))]
                                     (if (coords [x y]) "#" "."))))))
       "\n\n"))

(defn- print-grid [coords]
  (let [x-max (apply max (map first coords))
        x-min (apply min (map first coords))
        y-max (apply max (map second coords))
        y-min (apply min (map second coords))]
    (for [layer (sort (group-by #(nth % 2) (sort coords)))]
      (print-layer layer [x-min x-max y-min y-max]))))

(comment
  (println (str/join (print-grid (last (take 7 (iterate step (parse-input (slurp "resources/day17input"))))))))
  (count (last (take 7 (iterate step (parse-input (slurp "resources/day17input")))))))
