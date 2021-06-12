(ns aoc2020.day23-2)

(def input [7 1 2 6 4 3 5 8 9])
(def example [3 8 9 1 2 5 4 6 7])

(def long-ex (int-array (concat example (range 10 1000001))))

(defn target [current pickups max]
  (cond ((set pickups) (dec current)) (recur (dec current) pickups max)
        (< (dec current) 1) (recur max pickups max)
        :else (dec current)))

(defn index-of
  ([n is] (index-of n is 0))
  ([n is guess]
   (if (= n (aget is guess))
     guess
     (recur n is (inc guess)))))

(def example! (int-array example))
(def ex-before [3 2 5 4 6 7 8 9 1])
(def example! (int-array ex-before))

(comment
  (let [idx 2
        current (aget example! idx)
        pickup [(aget example! (+ 1 idx))
                (aget example! (+ 2 idx))
                (aget example! (+ 3 idx))]
        tgt-idx (index-of (target current pickup 9) example!)
        shifts (Math/abs (- idx tgt-idx))]
    (tap> {:idx idx :current current :p pickup :tgt-idx tgt-idx
           :shifts shifts
           :a (- idx tgt-idx)})
    (tap> (range (inc idx) (- tgt-idx 2)))
    (tap> (range (+ 4 idx) (inc tgt-idx)))
    (if (< idx tgt-idx)
      (do  (doall (map #(aset-int example! %1 (aget example! %2))
                       (range (+ 1 idx) (- tgt-idx 2))
                       (range (+ 4 idx) (inc tgt-idx))))
           (doall (map #(aset-int example! %1 %2)
                       (range (- tgt-idx 2) (+ tgt-idx 2))
                       pickup)))
      (do (doall (map #(aset-int example! %1 (aget example! %2))
                      (reverse (range (+ 4 tgt-idx) (+ tgt-idx shifts 4)))
                      (reverse (range (+ 1 tgt-idx) (+ tgt-idx shifts 1)))))
          (doall (map #(aset-int example! %1 %2)
                      (range (inc tgt-idx) (+ 4 tgt-idx))
                      pickup))))))

(vec example!)

(let [idx 2
      tgt-idx 0
      shifts (- idx tgt-idx)]
  [(reverse (range (+ 4 tgt-idx) (+ tgt-idx shifts 4)))
   (reverse (range (+ 1 tgt-idx) (+ tgt-idx shifts 1)))
   (range (inc tgt-idx) (+ 4 tgt-idx))])

(let [idx 0
      tgt-idx 4
      shifts (- tgt-idx idx 3)]
  [(range (+ 1 idx) (+ 1 idx shifts))
   (range (+ 4 idx) (+ 4 idx shifts))])

