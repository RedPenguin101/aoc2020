(ns day14
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn- binary-array
  ([n] (map #(Character/digit % 10) (Integer/toBinaryString n)))
  ([n length] (let [ns (binary-array n)]
                (concat (repeat (- length (count ns)) 0) ns))))

(defn- apply-mask [mask array]
  (Long/parseLong (apply str (map #(or %1 %2) mask array)) 2))

(defn- parse-instruction [string]
  (map #(Integer/parseInt %) (drop 1 (re-matches #"mem\[(\d+)\] = (\d+)" string))))

(defn- parse-mask [string]
  (->> string
       (re-matches #"mask = (\w+)")
       second
       (map #(Character/digit % 10))
       (map #(when (not= -1 %) %))))

(defn- update-memory [state line]
  (if (= "mem" (subs line 0 3))
    (let [[mem-loc number] (parse-instruction line)]
      (assoc-in state [:memory mem-loc] (apply-mask (:mask state) (binary-array number 36))))
    (assoc state :mask (parse-mask line))))

(->> "resources/day14input"
     slurp
     str/split-lines
     (reduce update-memory {})
     :memory
     vals
     (apply +))

(defn- apply-mask-v2 [mask bin-array]
  (let [i (volatile! -1)]
    (map #(case %1
            0 %2
            1 1
            nil (do
                  (vswap! i inc)
                  (vector :r @i)))
         mask bin-array)))

(defn- memory-addresses [result]
  (let [replacements (filter coll? result)
        n (count replacements)]
    (map (comp #(Long/parseLong % 2) #(apply str %))
         (map #(replace % result)
              (map #(zipmap replacements %) (combo/permuted-combinations (apply concat [(repeat n 0) (repeat n 1)]) n))))))

(defn- mem-push [mem locs val]
  (reduce (fn [m loc]
            (assoc m loc val))
          mem
          locs))

(defn- update-memory-v2 [{:keys [mask memory] :as state} line]
  (if (= "mem" (subs line 0 3))
    (let [[mem-loc number] (parse-instruction line)]
      (assoc state :memory (mem-push memory
                                     (memory-addresses (apply-mask-v2 mask (binary-array mem-loc 36)))
                                     number)))
    (assoc state :mask (parse-mask line))))

(->> "resources/day14input"
     slurp
     str/split-lines
     (reduce update-memory-v2 {})
     :memory
     vals
     (apply +))
