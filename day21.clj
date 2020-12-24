(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn map-kv [f coll]
  (into {} (map (fn [[k v]] [k (f v)]) coll)))

(defn unwrap [x]
  (if (= 1 (count x))
    (first x)
    x))

(defn line->recipe
  "Produces a recipe - a tuple of [ingredients allergens]"
  [line]
  (let [[ingredients allergens] (str/split line #"\(contains")]
    [(re-seq #"\w+" ingredients) (re-seq #"\w+" allergens)]))

(def recipes (map line->recipe (str/split-lines (slurp "resources/day21example"))))
(def input (str/split-lines (slurp "resources/day21input")))

(defn allergen-candidates
  "Returns a map of allergen->candidate ingredients"
  [recipes]
  (reduce (fn [A [ingredients allergens]]
            (merge-with set/intersection A (into {} (for [allergen allergens]
                                                      [allergen (set ingredients)]))))
          {}
          recipes))

(defn- narrow-down
  "Given a sequence of sets of possible label names, will successively narrow down the 
   possibilties until only a single possible solution remains, and return that solution."
  ([candidates] (narrow-down candidates 0))
  ([candidates it]
   (let [settled (apply set/union (filter #(= 1 (count %)) (vals candidates)))]
     (cond (> it 100) :break
           (= (count settled) (count candidates)) (map-kv unwrap candidates)
           :else (recur (map-kv #(if (= 1 (count %))
                                   %
                                   (set/difference % settled)) candidates)
                        (inc it))))))

(comment
  "one food per line
 ingredient list (word) followed by some or all of the allergens (not always marked)
 1 allegen has exactly 1 ingredient
 Part1: What ingredients don't contain any allergens?
 Part2: what ingredient contains which allergen"

  (line->recipe "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)")
  ;; => [("mxmxvkd" "kfcds" "sqjhc" "nhms") ("dairy" "fish")]

  "Taking kfcds:
   It's not in recipe 2, which has dairy, so its allergen can't be dairy
   It's not in recipe 4, which has fish, so its allergen can't be fish
   It's not in recipe 3, which has soy, so its allergen can't be soy
   therefore it doesn't have any allergens
   
   or flipped around: 
   * dairy is in recipe1: mxmxvkd kfcds sqjhc nhms
   * dairy is in recipe1: trh fvjkl sbzzf mxmxvkd
   
   Dairy must be one of the ingredients that are in the intersection of these recipes"

  (allergen-candidates recipes)
  {"dairy" #{"mxmxvkd"}, "fish" #{"sqjhc" "mxmxvkd"}, "soy" #{"sqjhc" "fvjkl"}}

  (set/difference
   (set (mapcat first recipes))
   (apply set/union (vals (allergen-candidates recipes))))
  #{"nhms" "trh" "kfcds" "sbzzf"}

  (count (filter (set/difference
                  (set (mapcat first recipes))
                  (apply set/union (vals (allergen-candidates recipes))))
                 (mapcat first recipes)))
  ;; => 5

  "Part 1"
  (time (let [recipes (map line->recipe input)]
          (count (filter (set/difference
                          (set (mapcat first recipes))
                          (apply set/union (vals (allergen-candidates recipes))))
                         (mapcat first recipes)))))
  ;; => 1882

  "Part 2"
  (str/join "," (vals (sort (narrow-down (allergen-candidates (map line->recipe input)))))))
