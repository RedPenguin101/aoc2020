(ns day22
  (:require [clojure.string :as str]))

(def input (slurp "resources/day22input"))
(def example (slurp "resources/day22example"))

(defn input->hands [input]
  (->> (str/split input #"\n\n")
       (map (partial re-seq #"\d+"))
       (map (partial drop 1))
       (map (partial map #(Integer/parseInt %)))
       (map #(apply conj (clojure.lang.PersistentQueue/EMPTY) %))))

(def example-start-hand (input->hands example))

(defn play-hand [[player1 player2]]
  (let [p1 (first player1)
        p2 (first player2)]
    (if (> p1 p2)
      [(conj (pop player1) p1 p2) (pop player2)]
      [(pop player1) (conj (pop player2) p2 p1)])))

(defn play-game [game]
  (if (some empty? game)
    (apply + (map * (seq (some not-empty game)) (reverse (range 1 (inc (count (some not-empty game)))))))
    (recur (play-hand game))))

(comment
  (play-game example-start-hand)
  (play-game (input->hands input)))

(defn winner [hand]
  (if (empty? (first hand)) :p2 :p1))

(defn sub-game-starting-hand [[old-p1 old-p2]]
  (list (apply conj (clojure.lang.PersistentQueue/EMPTY) (take (first old-p1) (pop old-p1)))
        (apply conj (clojure.lang.PersistentQueue/EMPTY) (take (first old-p2) (pop old-p2)))))

(defn score-hand [hand]
  [(winner hand)
   (apply + (map * (seq (some not-empty hand)) (reverse (range 1 (inc (count (some not-empty hand)))))))])

(defn apply-winner [[player1 player2] round-winner]
  (if (= :p1 round-winner)
    [(conj (pop player1) (first player1) (first player2)) (pop player2)]
    [(pop player1) (conj (pop player2) (first player2) (first player1))]))

(defn play-recursive-combat
  "Given an initial game state, returns a tuple of the winner of the game, and the
   last round of the game"
  [game]
  (let [[p1 p2 :as last-round] (last game)
        c1 (first p1) c2 (first p2)]
    (cond (some empty? last-round) [(winner last-round) last-round]
          (some #(= last-round %) (butlast game)) [:p1 last-round]

          (and (> (count p1) c1) (> (count p2) c2))
          (recur (conj game (apply-winner
                             last-round
                             (first ((memoize play-recursive-combat) [(sub-game-starting-hand last-round)])))))

          :else (recur (conj game (play-hand last-round))))))

(comment
  (score-hand (second (play-recursive-combat [example-start-hand])))
  (time (score-hand (second (play-recursive-combat [(input->hands input)]))))
  (time (score-hand (second ((memoize play-recursive-combat) [(input->hands input)])))))
