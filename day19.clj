(ns day19
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(let [input (str/split (slurp "resources/day19input") #"\n\n")
      rules (insta/parser (str/join "\n" (sort-by #(Integer/parseInt (first (str/split % #":"))) (str/split-lines (first input)))))]
  (count (remove :index (map rules (str/split-lines (second input))))))

(let [input (str/split (slurp "resources/day19input") #"\n\n")
      rules (insta/parser (str/join "\n" (sort-by #(Integer/parseInt (first (str/split % #":"))) 
                                                  (replace {"8: 42" "8: 42 | 42 8"
                                                            "11: 42 31" "11: 42 31 | 42 11 31"} 
                                                           (str/split-lines (first input))))))]
  (count (remove :index (map rules (str/split-lines (second input))))))
