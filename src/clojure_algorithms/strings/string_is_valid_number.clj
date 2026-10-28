(ns clojure-algorithms.strings.string-is-valid-number
  "Valid-number check via a deterministic finite automaton (LeetCode 65).
  Ported from TheAlgorithms/Python.")

(def state-machine
  {:initial {:numeric :whole :sign :signed :decimal :fractional}
   :signed {:numeric :whole :decimal :fractional}
   :whole {:numeric :whole :decimal :fraction :exponent :exponential}
   :fractional {:numeric :fraction}
   :fraction {:numeric :fraction :exponent :exponential}
   :exponential {:numeric :exp-number :sign :exp-sign}
   :exp-sign {:numeric :exp-number}
   :exp-number {:numeric :exp-number}})

(def ^:private valid-final-states #{:whole :fraction :exp-number})

(defn classify-char
  "Classifies single-char string `ch` as :numeric/:sign/:exponent/:decimal,
  or nil."
  [ch]
  (when (= 1 (count ch))
    (let [c (first ch)]
      (cond
        (Character/isDigit ^char c) :numeric
        (contains? #{\+ \-} c) :sign
        (contains? #{\e \E} c) :exponent
        (= c \.) :decimal
        :else nil))))

(defn is-valid-number
  "True when `s` is a valid number literal."
  [s]
  (loop [chars (seq s) state :initial]
    (if (nil? chars)
      (contains? valid-final-states state)
      (let [t (classify-char (str (first chars)))
            nxt (get-in state-machine [state t])]
        (if (or (nil? t) (nil? nxt))
          false
          (recur (next chars) nxt))))))
