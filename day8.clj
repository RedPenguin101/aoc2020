(ns day8
  (:require [clojure.string :as str]))

(defn parse-instruction [line]
  (let [[op num] (str/split line #" ")]
    [(keyword op) (Integer/parseInt num)]))

(defn create-state [instructions]
  {:instructions instructions
   :accumulator 0
   :pointer 0
   :execution-history []})

(defn execute-instruction [{:keys [pointer instructions] :as state}]
  (if-let [instr (nth instructions pointer nil)]
    (-> state
        (update :accumulator #(if (= (first instr) :acc) (+ % (second instr)) %))
        (update :pointer #(if (= (first instr) :jmp) (+ % (second instr)) (inc %)))
        (update :execution-history conj pointer))
    (assoc state :terminated true)))

(defn no-loop? [state]
  (not ((set (:execution-history state)) (:pointer state))))

(def instructions (mapv parse-instruction (str/split-lines (slurp "resources/day8input"))))

(comment
  (def example-state (create-state (mapv parse-instruction (str/split-lines (slurp "resources/day8example")))))
  
  (execute-instruction example-state)

  (:accumulator (last (take-while no-loop? (iterate execute-instruction example-state))))

  (->> instructions
       create-state
       (iterate execute-instruction)
       (take-while no-loop?)
       last
       :accumulator))

(defn run-to-loop-or-termination [state]
  (cond (:terminated state) state
        (no-loop? state) (recur (execute-instruction state))
        :else (assoc state :looped true)))

(defn accumulator-at-termination [instructions]
  (let [result (run-to-loop-or-termination (create-state instructions))]
    (when (:terminated result) (:accumulator result))))

(defn swap-jpm-nop [[idx instr]]
  (when (#{:jmp :nop} (first (nth instr idx)))
    (update instr idx (fn [[op arg]] [(op {:jmp :nop :nop :jmp}) arg]))))

(defn instruction-variants [f instructions]
  (keep f (map #(vector % instructions) (range (count instructions)))))

(comment
  (run-to-loop-or-termination example-state)

  (run-to-loop-or-termination {:instructions [[:nop 0] [:acc 1] [:jmp 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:nop -4] [:acc 6]]
                               :accumulator 0
                               :pointer 0
                               :execution-history []})

  (accumulator-at-termination [[:nop 0] [:acc 1] [:jmp 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:jmp -4] [:acc 6]])
  (accumulator-at-termination [[:nop 0] [:acc 1] [:jmp 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:nop -4] [:acc 6]])

  (def ex-instr [[:nop 0] [:acc 1] [:jmp 4] [:acc 3] [:jmp -3] [:acc -99] [:acc 1] [:jmp -4] [:acc 6]])

  (some accumulator-at-termination (instruction-variants swap-jpm-nop ex-instr))
  
  (->> instructions
       (instruction-variants swap-jpm-nop)
       (some accumulator-at-termination)))
