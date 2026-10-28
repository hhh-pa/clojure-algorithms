(ns clojure-algorithms.dynamic-programming.viterbi
  "Most likely state path for observations (Viterbi algorithm).
  Ported from TheAlgorithms/Python dynamic_programming/viterbi.py.")

(defn- validate-list
  "Throw unless `value` is a sequential of strings."
  [value var-name]
  (when-not (sequential? value)
    (throw (IllegalArgumentException. (str var-name " must be a list"))))
  (when-not (every? string? value)
    (throw (IllegalArgumentException. (str var-name " must be a list of strings")))))

(defn- validate-dict
  "Throw unless `value` is a map of string keys to `value-pred` values."
  [value var-name value-pred type-name nested?]
  (when-not (map? value)
    (throw (IllegalArgumentException. (str var-name " must be a dict"))))
  (when-not (every? string? (keys value))
    (throw (IllegalArgumentException. (str var-name " all keys must be strings"))))
  (when-not (every? value-pred (vals value))
    (throw (IllegalArgumentException.
            (str var-name " " (when nested? "nested dictionary ")
                 "all values must be " type-name)))))

(defn- validate-nested-dict
  "Throw unless `value` is a map of string keys to float-valued maps."
  [value var-name]
  (validate-dict value var-name map? "dict" false)
  (doseq [inner (vals value)]
    (validate-dict inner var-name float? "float" true)))

(defn- validate
  "Throw unless all Viterbi inputs are well-formed."
  [observations states initial transition emission]
  (when-not (and (seq observations) (seq states) (seq initial)
                 (seq transition) (seq emission))
    (throw (IllegalArgumentException. "There's an empty parameter")))
  (validate-list observations "observations_space")
  (validate-list states "states_space")
  (validate-dict initial "initial_probabilities" float? "float" false)
  (validate-nested-dict transition "transition_probabilities")
  (validate-nested-dict emission "emission_probabilities"))

(defn viterbi
  "Most likely state sequence for `observations`."
  [observations states initial-probs transition-probs emission-probs]
  (validate observations states initial-probs transition-probs emission-probs)
  (let [probs (atom {})
        pointers (atom {})]
    (doseq [state states]
      (let [obs (first observations)]
        (swap! probs assoc [state obs]
               (* (initial-probs state) (get-in emission-probs [state obs])))
        (swap! pointers assoc [state obs] nil)))
    (doseq [o (range 1 (count observations))]
      (let [obs (nth observations o)
            prior (nth observations (dec o))]
        (doseq [state states]
          (let [[arg-max _] (reduce (fn [[best-s best-p] k-state]
                                      (let [p (* (@probs [k-state prior])
                                                 (get-in transition-probs [k-state state])
                                                 (get-in emission-probs [state obs]))]
                                        (if (> p best-p) [k-state p] [best-s best-p])))
                                    ["" -1]
                                    states)]
            (swap! probs assoc [state obs]
                   (* (@probs [arg-max prior])
                      (get-in transition-probs [arg-max state])
                      (get-in emission-probs [state obs])))
            (swap! pointers assoc [state obs] arg-max)))))
    (let [final-obs (last observations)
          last-state (first (reduce (fn [[best-s best-p] k-state]
                                      (let [p (@probs [k-state final-obs])]
                                        (if (> p best-p) [k-state p] [best-s best-p])))
                                    ["" -1]
                                    states))]
      (loop [o (dec (count observations)) prev last-state result []]
        (if (neg? o)
          (vec (reverse result))
          (recur (dec o) (@pointers [prev (nth observations o)])
                 (conj result prev)))))))
