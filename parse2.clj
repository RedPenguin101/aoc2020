(ns parse2
  (:require [clojure.string :as str]))

(defn parse [s]
  (read-string (str "(" (str/escape s {\( " (:either "
                                       \) ")"
                                       \| " :or "})
                    ")")))

(parse "N(E|W)N")
(parse "N(E|W(W|E))N")
;; => (N (E :or W (W :or E)) N)

'(W :or E)

'(A (:either B :or C (:either D :or E)) F)
'(A (:either B :or C ([D] [E])) F)
'(A (:either B :or ([CD] [CE])) F)
'(A ([B] [CD] [CE]) F)
'([ABF] [ACDF] [ACEF])


'(A (:choose B :or C (:choose D :or E)) F)
'(A (:choose B :or C ([D] [E])) F)
'(A (:choose B :or ([CD] [CE])) F)
'(A ([B] [CD] [CE]) F)
'([ABF] [ACDF] [ACEF])

(.indexOf '(a b :or c) :or)

(defn e [x]
  (cond (not (seq? x)) x
        (= :choose (first x)) (map e (split-at (.indexOf (rest x) :or) (rest x)))
        (= :or (first x)) (map e (rest x))
        :else (map e x)))

(e '(A (:choose B :or C (:choose D :or E)) F))

'(A (:choose B :or C (:choose D :or E)) F)

'(:choose D :or E) ; evals to ([D] [E])
'(:choose B :or C ([D] [E])) ; C ([D] [E]) evals to ([CD] [CE])


'(A (:choose B :or C ([D] [E])) F)

'[[A B F]
  [A C D F]
  [A C E F]]

(:either form1 :or form2)
[(f form1) (f form2)]

