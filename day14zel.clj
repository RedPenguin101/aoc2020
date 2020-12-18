(ns day14zel
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp "resources/day14input"))

(defn parse-mask [mask]
  {:and      (Long/parseLong (str/escape mask {\1 \0, \X \1}) 2)
   :or       (Long/parseLong (str/escape mask {\X 0}) 2)
   :floating (keep-indexed #(when (= %2 \X) (- 35 %1)) mask)}) ; always 36 bits

(parse-mask "01X11X10X10110110X111X11010X1X101010")
;; => {:and 9798172992, :or 24254854314, :floating (33 30 27 18 14 8 6)}


(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-matches #"(mask|mem)(?:\[(\d+)\])? = ([X01]+|\d+)" %))
       (map (fn [[_ cmd a b]]
              (case cmd
                "mask" [:mask (parse-mask b)]
                "mem"  [:mem (Long/parseLong a) (Long/parseLong b)])))))

(parse-input input)
;; => ([:mask {:and 9798172992, :or 24254854314, :floating (33 30 27 18 14 8 6)}]
;;     [:mem 19409 3025]
;;     [:mem 40104 798480382]
;;     [:mem 25359 905]
;;     [:mask {:and 1086492672, :or 24632183808, :floating (30 23 22 17 15 12)}]
;;     [:mem 55479 930785]
;;     [:mem 25548 130263864]
;;     ...)

(let [[cmd a b] [:mask {:and 9798172992, :or 24254854314, :floating '(33 30 27 18 14 8 6)}]]
  [cmd a b])
;; => [:mask {:and 9798172992, :or 24254854314, :floating (33 30 27 18 14 8 6)} nil]


(map-indexed (fn [idx val] (if (= val \X) (- 35 idx) :this-isnt-x)) "01X11X10X10110110X111X11010X1X101010")


(defn run-program [re-mem code]
  (loop [commands code mask {} mem {}]
    (if-let [[cmd a b] (first commands)]
      (case cmd
        :mask (recur (next commands) a mem)
        :mem  (recur (next commands) mask (re-mem mem mask a b)))
      (apply + (vals mem)))))

;; part 1
(defn apply-mask-v1 [mask number]
  (bit-or (bit-and number (mask :and))
          (mask :or)))

(defn mem-v1 [mem mask address value]
  (assoc mem address (apply-mask-v1 mask value)))

(run-program mem-v1 (parse-input input)) ; 13476250121721

;; part 2
(defn apply-floating [floating fmask number]
  (reduce (fn [num [i1 i2]]
            (if (bit-test fmask i1) (bit-set num i2) (bit-clear num i2)))
          number
          (map-indexed vector floating)))

(defn apply-mask-v2 [{:keys [floating] :as mask} number]
  (->> (bit-or number (mask :or))
       (repeat)
       (map (partial apply-floating floating)
            (range (bit-shift-left 1 (count floating))))))

(defn mem-v2 [mem mask address value]
  (reduce #(assoc %1 %2 value) mem (apply-mask-v2 mask address)))

(run-program mem-v2 (parse-input input)) ; 4463708436768