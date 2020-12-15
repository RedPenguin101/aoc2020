(ns day15)

(def input [2,15,0,9,1,20])
(def ex [0,3,6])

(defn prep [nums]
  {:turn (count nums)
   :last-spoken-on (into {} (map-indexed #(vector %2 (inc %1)) (butlast nums)))
   :number (last nums)})

(defn next-number [{:keys [turn number] :as x}]
  (let [last-spoken-on (get-in x [:last-spoken-on number])]
    (-> x
        (assoc-in [:last-spoken-on number] turn)
        (assoc :number (if last-spoken-on (- turn last-spoken-on) 0))
        (update :turn inc))))

(aset-int (int-array [1 2 3]) 0 5)

(comment
  (time (:number (first (drop-while #(not= 2020 (:turn %))  (iterate next-number (prep input))))))
  (time (:number (first (drop-while #(not= 30000000 (:turn %))  (iterate next-number (prep input)))))))

(defn update-last-spoken! [array n turn]
  (aset-int array n turn))

(defn f [array xs]
  (if (pos? (count xs))
    (do (update-last-spoken! array (second (first xs)) (ffirst xs))
        (recur array (rest xs)))
    array))

(defn last-spoken-on [array n]
  (let [lso (aget array n)]
    (when (pos? lso) lso)))

(defn prep2 [nums last-spoken-array]
  (f last-spoken-array (map-indexed #(vector (inc %1) %2) (butlast nums)))
  
  {:turn (count nums)
   :number (last nums)
   :last-spoken last-spoken-array})

(defn next-number2 [{:keys [turn number last-spoken] :as x}]
  (let [last-spoken-on (last-spoken-on last-spoken number)]
    (update-last-spoken! last-spoken number turn)
    (-> x
        (assoc :number (if last-spoken-on (- turn last-spoken-on) 0))
        (update :turn inc))))

(comment
  (time (let [a (int-array 1000000 0)]
          (:number (first (drop-while #(not= 2020 (:turn %))  (iterate next-number2 (prep2 input a)))))))

  (time (let [a (int-array 30000000 0)]
          (:number (first (drop-while #(not= 30000000 (:turn %))  (iterate next-number2 (prep2 input a)))))))

  ;; zelarks solution
  (defn solve-fast [numbers ^long limit]
    (let [spoken (int-array limit)
          _      (doseq [[^int i num] (map-indexed vector numbers)]
                   (aset spoken num (inc i)))]
      (loop [turn   (int (count numbers))
             number (int (peek numbers))]
        (if (== turn limit)
          number
          (let [last-spoken (aget spoken number)
                _           (aset spoken number turn)]
            (recur (unchecked-inc-int turn)
                   (cond->> last-spoken
                     (not= last-spoken 0) (unchecked-subtract-int turn))))))))

  (time (solve-fast input 30000000)))