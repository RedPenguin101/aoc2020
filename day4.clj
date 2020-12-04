(ns day4
  (:require [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.spec.alpha :as s]))

(defn input-parse [input]
  (map #(str/split % #"[ \n]") (str/split input #"\n\n")))

(defn mapify-passport [strs]
  (->> strs
       (map #(str/split % #":"))
       (into {})
       (keywordize-keys)))

(defn valid-passport? [passport-map]
  (every? #(contains? passport-map %) [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(comment
  "Non-spec method"
  
  (map mapify-passport (input-parse (slurp "resources/day4example")))

  (valid-passport? {:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"})
  (valid-passport? {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"})
  (valid-passport? {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"})
  (valid-passport? {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"})

  "Part 1 solution"

  (->> (slurp "resources/day4input")
       (input-parse)
       (map mapify-passport)
       (map valid-passport?)
       (remove false?)
       count))

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(comment
  "With Spec"
  
  (->> (slurp "resources/day4input")
       (input-parse)
       (map mapify-passport)
       (map (partial s/valid? ::passport))
       (remove false?)
       count))

(defn parsed-between? [a b x]
  (<= a (try (Integer/parseInt x) (catch NumberFormatException e 0)) b))

(s/def ::byr #(parsed-between? 1920 2002 %))
(s/def ::iyr #(parsed-between? 2010 2020 %))
(s/def ::eyr #(parsed-between? 2020 2030 %))

(defn valid-height? [s]
  (let [[num type] (map #(apply str %) (split-at (- (count s) 2) s))]
    (cond (= type "cm") (parsed-between? 150 193 num)
          (= type "in") (parsed-between? 59 76 num)
          :else false)))

(s/def ::hgt valid-height?)
(s/def ::hcl #(re-matches #"#\w{6}" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"\d{9}" %))

(comment
  "Part 2 examples"
  
  (->> (slurp "resources/day4example2valid")
       (input-parse)
       (map mapify-passport)
       (map (partial s/valid? ::passport))
       (every? true?))

  (->> (slurp "resources/day4example2invalid")
       (input-parse)
       (map mapify-passport)
       (map (partial s/valid? ::passport))
       (every? false?))
  
  "Part 2 solution"
  
  (->> (slurp "resources/day4input")
       (input-parse)
       (map mapify-passport)
       (map (partial s/valid? ::passport))
       (remove false?)
       count))