(ns aoc2020.day25)

(def pk_c 12320657)
(def pk_d 9659666)

(defn transform [sn]
  (fn [val]
    (mod (* val sn) 20201227)))

(defn loop-size
  ([sn pk]
   (loop-size (transform sn) pk 1 0))
  ([xf pk val lsize]
   (cond
     (> lsize 10000000) :break
     (= val pk) lsize
     :else (recur xf pk (xf val) (inc lsize)))))

(comment
  (loop-size 7 5764801)
  (loop-size 7 17807724)


  (last (take (inc 11) (iterate (transform 5764801) 1)))
  ((transform 17807724) 8))

(defn find-ek [pk_c pk_d]
  (let [lsize_c (loop-size 7 pk_c)]
    (last (take (inc lsize_c) (iterate (transform pk_d) 1)))))

(comment
  (find-ek 5764801 17807724)
  (find-ek 12320657 9659666)

  (loop-size 5 12320657))