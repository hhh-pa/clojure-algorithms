(ns clojure-algorithms.conversions.decimal-to-any
  "Port of conversions/decimal_to_any.py.
  Converts a positive integer to any base (2-36) string.")

(def ^:private digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn decimal-to-any
  "Convert non-negative integer NUM to a base-BASE string."
  [num base]
  (when (float? num)
    (throw (ex-info "int() can't convert non-string with explicit base"
                    {:type :type-error})))
  (when (neg? num)
    (throw (ex-info "parameter must be positive int" {:type :value-error})))
  (when (string? base)
    (throw (ex-info "'str' object cannot be interpreted as an integer"
                    {:type :type-error})))
  (when (float? base)
    (throw (ex-info "'float' object cannot be interpreted as an integer"
                    {:type :type-error})))
  (when (#{0 1} base)
    (throw (ex-info "base must be >= 2" {:type :value-error})))
  (when (> base 36)
    (throw (ex-info "base must be <= 36" {:type :value-error})))
  (if (zero? num)
    "0"
    (loop [n num acc ""]
      (if (zero? n)
        acc
        (recur (quot n base) (str (nth digits (rem n base)) acc))))))
