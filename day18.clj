(ns day18
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn parse-line [string]
  (edn/read-string (str "(" string ")")))

(def fns {'+ +
          '* *})

(defn evaluate [form]
  (if (number? form) form
      (let [[a1 op a2 & rest] form]
        (if op (recur (cons ((fns op) (evaluate a1) (evaluate a2)) rest))
            a1))))

(deftest t
  (is (= (evaluate '(1)) 1))
  (is (= 3 (evaluate '(1 + 2))))
  (is (= 9 (evaluate '(1 + 2 * 3))))
  (is (= 71 (evaluate '(1 + 2 * 3 + 4 * 5 + 6))))
  (is (= 7 (evaluate '(1 + (2 * 3)))))
  (is (= 51 (evaluate '(1 + (2 * 3) + (4 * (5 + 6))))))
  (is (= 26 (evaluate '(2 * 3 + (4 * 5)))))
  (is (= 437 (evaluate '(5 + (8 * 3 + 9 + 3 * 4 * 3)))))
  (is (= 12240 (evaluate '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))))))
  (is (= 13632 (evaluate '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2))))

  (is (= '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2) (parse-line "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

(apply + (map (comp evaluate parse-line) (str/split-lines (slurp "resources/day18input"))))
;; => 98621258158412

(defn evaluate-2 [form]
  (if (number? form) form
      (let [[a1 op a2 & rest] form]
        (cond
          (and (= '* op) (= '+ (first rest))) 
          (recur (concat (list a1 op 
                               (+ (evaluate-2 a2) (evaluate-2 (second rest)))) 
                         (drop 2 rest)))
          
          op
          (recur (cons ((fns op) (evaluate-2 a1) (evaluate-2 a2)) rest))
          
          :else a1))))

(deftest t2
  (is (= 3 (evaluate-2 '(1 + 2))))
  (is (= 4 (evaluate-2 '(2 * 2))))
  (is (= 10 (evaluate-2 '(2 * 2 + 3))))
  (is (= 9 (evaluate-2 '(1 + 2 * 3))))
  (is (= 231 (evaluate-2 '(1 + 2 * 3 + 4 * 5 + 6))))
  (is (= 51 (evaluate-2 '(1 + (2 * 3) + (4 * (5 + 6))))))
  (is (= 46 (evaluate-2 '(2 * 3 + (4 * 5)))))
  (is (= 1445 (evaluate-2 '(5 + (8 * 3 + 9 + 3 * 4 * 3)))))
  (is (= 669060 (evaluate-2 '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))))))
  (is (= 23340 (evaluate-2 '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)))))

(apply + (map (comp evaluate-2 parse-line) (str/split-lines (slurp "resources/day18input"))))
;; => 241216538527890
