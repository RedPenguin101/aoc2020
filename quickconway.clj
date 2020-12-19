(ns quickconway)

(def blinker [[0 0] [0 1] [0 2]])

(defn neighbours [[x y]]
  (for [xvar [-1 0 1]
        yvar [-1 0 1]
        :when (not= xvar yvar 0)]
    [(+ x xvar) (+ y yvar)]))

(defn map-invert-multi [m]
  (reduce (fn [n [key val]] (update n val conj key)) {} m))

(defn new-active-cells [already-active-cells]
  (let [cells-with-n-active-neigbours
        (->> already-active-cells
             (mapcat neighbours)
             frequencies
             map-invert-multi)]
    
    (sort (concat (get cells-with-n-active-neigbours 3)
                  (filter (set already-active-cells) (get cells-with-n-active-neigbours 2))))))

(-> blinker
    new-active-cells
    new-active-cells)
