(ns day20-2
  (:require [clojure.test :refer [deftest is]]))

(defn flip [tile dir]
  (case dir
    :fv (reverse tile)
    :fh (map reverse tile)
    :fd1 (apply map vector tile)
    :fd2 (flip (flip tile :r1) :fv)
    :r0 tile
    :r1 (apply map vector (reverse tile))
    :r2 (flip (flip tile :r1) :r1)
    :r3 (flip (flip tile :fh) :fd1)))


(deftest tile-flipping
  (let [tile [[:a :b :c]
              [:d :e :f]
              [:g :h :i]]]
    (is (= [[:c :b :a]
            [:f :e :d]
            [:i :h :g]]
           (flip tile :fh)))
    (is (= [[:g :h :i]
            [:d :e :f]
            [:a :b :c]]
           (flip tile :fv)))
    (is (= [[:a :b :c]
            [:d :e :f]
            [:g :h :i]]
           (flip tile :r0)))
    (is (= [[:g :d :a]
            [:h :e :b]
            [:i :f :c]]
           (flip tile :r1)))
    (is (= [[:i :h :g]
            [:f :e :d]
            [:c :b :a]]
           (flip tile :r2)))
    (is (= [[:i :f :c]
            [:h :e :b]
            [:g :d :a]]
           (flip tile :fd2)))
    (is (= [[:c :f :i]
            [:b :e :h]
            [:a :d :g]]
           (flip tile :r3)))))