(ns clojure-algorithms.boolean-algebra.multiplexer
  "2-to-1 multiplexer.")

(defn mux
  "Returns input1 when select is 1, else input0. All args must be 0 or 1."
  [input0 input1 select]
  (if (every? #{0 1} [input0 input1 select])
    (if (= 1 select) input1 input0)
    (throw (IllegalArgumentException.
            "Inputs and select signal must be 0 or 1"))))
