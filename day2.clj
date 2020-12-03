(ns day2
  (:require [clojure.string :as str]))

"https://adventofcode.com/2020/day/2"

(def a {:policy {:lower-freq 1 
                 :upper-freq 3 
                 :char \a}
        :password "abcde"})

(def b {:range [1 2]
        :char \a
        :password "abcde"})

(defn parse-input-row [s]
  (let [[range c pwd] (str/split s #" ")]
    {:range (map #(Integer/parseInt %) (str/split range #"-"))
     :char (first c)
     :password pwd}))

"The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid."

(defn password-check [{:keys [range char password]}]
  (<= (first range)
      (get (frequencies password) char 0)
      (second range)))

(comment
  (password-check (parse-input-row "14-15 v: hvhvlhvvvwxvdvscdpvg"))

  (password-check {:range [1 3]
                   :char \a
                   :password "abcde"})
  (password-check {:range [1 3]
                   :char \b
                   :password "cdefg"})
  (password-check {:range [2 9]
                   :char \c
                   :password "ccccccccc"})

  (->> "resources/day2input"
       slurp
       str/split-lines
       (map parse-input-row)
       (filter password-check)
       count))

(defn password-check2 [{:keys [range char password]}]
  (-> (map (comp (partial get password) dec) range)
      (frequencies)
      (get char)
      (= 1)))

(comment
  (password-check2 {:range [1 3]
                    :char \a
                    :password "abcde"})
  (password-check2 {:range [1 3]
                    :char \b
                    :password "cdefg"})
  (password-check2 {:range [2 9]
                    :char \c
                    :password "ccccccccc"})

  (->> "resources/day2input"
       slurp
       str/split-lines
       (map parse-input-row)
       (filter password-check2)
       count))
