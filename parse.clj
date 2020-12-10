(ns parse
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [ubergraph.core :as ug]))


(def x "ENWWW(NEEE|SSE(EE|N))")

(defn parse [s]
  (read-string (str "(" (str/escape s {\( " ("
                                       \) ")"
                                       \| " )( "})
                    ")")))

(parse "N(E|W)N")
;; => (N (E) (W) N)

;; ((N (E) (W)) N)

(defn edges [[parent & children]]
  (mapcat (fn [child] (when (seq? child) (cons [parent (first child)] (edges child)))) children))

(edges (parse "N(E|W)N"))

(defn words [graph node]
  (let [children (ug/successors graph node)]
    (if children
      (map #(str (name node) %) (flatten (map (partial words graph) children)))
      (name node))))

(let [a (parse x)]
  (words (apply ug/digraph (edges a)) (first a)))

(edges (parse "ENWWW(NEEE|SSE(EE|N))"))
(parse "ENNWSWW(NEWS|)SSSEEN")
(let [a (parse "N(E|W)")]
  (words (apply ug/digraph (edges a)) (first a)))
