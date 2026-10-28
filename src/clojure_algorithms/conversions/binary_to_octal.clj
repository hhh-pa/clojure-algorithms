(ns clojure-algorithms.conversions.binary-to-octal
  "Port of conversions/binary_to_octal.py.
  Converts a binary string to its octal equivalent.")

(defn bin-to-octal
  "Convert binary string BIN-STRING to an octal digit string."
  [bin-string]
  (when-not (every? #{\0 \1} bin-string)
    (throw (ex-info "Non-binary value was passed to the function"
                    {:type :value-error})))
  (when (empty? bin-string)
    (throw (ex-info "Empty string was passed to the function"
                    {:type :value-error})))
  (let [padded (str (apply str (repeat (mod (- 3 (mod (count bin-string) 3)) 3)
                                       \0))
                    bin-string)]
    (apply str (map (fn [grp]
                      (str (reduce (fn [acc [i ch]]
                                     (+ acc (* (long (Math/pow 2 (- 2 i)))
                                               (- (int ch) (int \0)))))
                                   0 (map-indexed vector grp))))
                    (partition 3 padded)))))
