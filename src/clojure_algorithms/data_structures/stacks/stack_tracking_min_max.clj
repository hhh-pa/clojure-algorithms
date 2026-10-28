(ns clojure-algorithms.data-structures.stacks.stack-tracking-min-max
  "Port of data_structures/stacks/stack_tracking_min_max.py.
  Stack tracking min/max. State is {:max-size n :stack [{:current :min :max}]}.
  Empty reads print \"Stack is empty\" and yield -infinity like Python.")

(defn make-min-max-stack
  "Min/max stack with MAX-STACK-SIZE slots (default 10)."
  ([] (make-min-max-stack 10))
  ([max-stack-size] {:max-size max-stack-size :stack []}))

(defn push-value
  "Push VALUE; returns [true new-stack]."
  [{:keys [max-size stack] :as st} value]
  (if (= (count stack) max-size)
    (throw (ex-info "Max stack size reached." {:type :exception}))
    (let [entry (if (empty? stack)
                  {:current value :min value :max value}
                  {:current value
                   :min (min (:min (peek stack)) value)
                   :max (max (:max (peek stack)) value)})]
      [true (update st :stack conj entry)])))

(defn- valid-or-inform [{:keys [stack]}]
  (if (empty? stack)
    (do (println "Stack is empty") false)
    true))

(defn pop-value
  "Pop; returns [value new-stack] (-infinity when empty)."
  [{:keys [stack] :as st}]
  (if (valid-or-inform st)
    [(:current (peek stack)) (update st :stack pop)]
    [##-Inf st]))

(defn get-current-max
  "Current maximum (-infinity when empty)."
  [st]
  (if (valid-or-inform st)
    (:max (peek (:stack st)))
    ##-Inf))

(defn get-current-min
  "Current minimum (-infinity when empty)."
  [st]
  (if (valid-or-inform st)
    (:min (peek (:stack st)))
    ##-Inf))
