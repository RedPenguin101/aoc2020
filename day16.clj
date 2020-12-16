(ns day16
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection difference union]]))

(defn- parse-rule [string]
  (let [matches (rest (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" string))]
    [(first matches) (partition 2 (map #(Integer/parseInt %) (rest matches)))]))

(defn- invalid-number?
  "Given rules and a number, will return the number if it's invalid, or nil if it is"
  [rules number]
  (when (not (some #(<= (first %) number (second %)) (apply concat (vals rules))))
    number))

(defn- valid-ticket? 
  "Given a ticket and rules, will return the ticket if it is valid, or nil if it's invalid"
  [rules ticket]
  (when (not-any? (partial invalid-number? rules) ticket)
    ticket))

(defn- parse-ticket [ticket-string]
  (mapv #(Integer/parseInt %) (str/split ticket-string #",")))

(defn- parse-input 
  "Returns a map of rules, valid-tickets and your-ticket"
  [input]
  (let [[rules your-ticket other-tickets] (str/split input #"\n\n")
        rules (into {} (map parse-rule (str/split-lines rules)))]
    {:rules rules
     :valid-tickets (filter (partial valid-ticket? rules) (map parse-ticket (rest (str/split-lines other-tickets))))
     :your-ticket (parse-ticket (second (str/split-lines your-ticket)))}))

(defn- options 
  "Given a map of rules and a number, will return a set of potentially valid labels names"
  [rules number]
  (set (filter seq (for [[label [[a b] [c d]]] rules]
                     (when (or (<= a number b) (<= c number d))
                       label)))))

(defn- narrow-down
  "Given a sequence of sets of possible label names, will find any positions that are 'settled'
   (i.e. have only one possible label) and remove those settles labels from every unsettled 
   position. This will recur until every position has been settled."
  ([xs] (narrow-down xs 0))
  ([xs it]
   (let [settled (apply union (filter #(= 1 (count %)) xs))]
     (cond (> it 100) :break
           (= (count settled) (count xs)) (map first xs)
           :else (recur (map #(if (= 1 (count %)) % (difference % settled)) xs)
                        (inc it))))))

(defn- find-labels
  "Given a map of rules, your ticket and other tickets, will find the labels for each position and
   return you ticket complete with labels"
  [{:keys [rules valid-tickets your-ticket]}]
  (zipmap (narrow-down (apply map intersection (map #(mapv (partial options rules) %) valid-tickets)))
          your-ticket))

(comment
  (def rules (into {} (vector (parse-rule "class: 1-3 or 5-7")
                              (parse-rule "row: 6-11 or 33-44")
                              (parse-rule "seat: 13-40 or 45-50"))))

  "Invalid-number returns nil if the number is valid, or the number if it's not"
  (invalid-number? rules 7)
  ;; => nil
  (invalid-number? rules 4)
  ;; => 4

  "solution to part 1"
  (time (apply + (keep (partial invalid-number? rules) [7,3,47 40,4,50 55,2,20 38,6,12])))
  ;; ~ 0.2ms
  ;; => 71

  "Valid ticket checks whether a ticket is valid"
  (valid-ticket? rules [7 3 47])
  ;; => [7 3 47]

  (valid-ticket? rules [40 4 50])
  ;; => nil

  "Parse input creates a structure with rules, your ticket and valid other tickets"
  (parse-input (slurp "resources/day16example"))
  ;; => {:rules {"class" ((1 3) (5 7)), "row" ((6 11) (33 44)), "seat" ((13 40) (45 50))},
  ;;     :valid-tickets ([7 3 47]),
  ;;     :your-ticket [7 1 14]}
       
  (def rules2 (:rules (parse-input (slurp "resources/day16example2"))))

  "options takes rules and a number, and returns a set of valid possible labels"
  (options rules2 3)
  ;; => #{"row" "seat"}
  (options rules2 15)
  ;; => #{"class" "row"}
  (options rules2 9)
  ;; => #{"class" "row" "seat"}

  "then you can map over it."
  (mapv (partial options rules2) [3 9 18])
  ;; => [#{"row" "seat"} #{"class" "row" "seat"} #{"class" "row" "seat"}]

  "Narrowing down recursively finds 'solved' positions and removes them from
   the other ones, until there's only one possible solution"
  (narrow-down '(#{"row"} #{"class" "row"} #{"class" "row" "seat"}))
  ;; => ("row" "class" "seat")

  "part2"
  (time (apply * (vals (filter (fn [[k _]] (= (subs k 0 3) "dep")) (find-labels (parse-input (slurp "resources/day16input")))))))
  ;; ~ 130ms
  ;; => 3765150732757
  )
