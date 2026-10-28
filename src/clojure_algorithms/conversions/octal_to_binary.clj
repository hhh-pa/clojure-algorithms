(ns clojure-algorithms.conversions.octal-to-binary
  "Port of conversions/octal_to_binary.py.
  Converts an octal string to its binary digit string.")

(def ^:private octal-digits #{\0 \1 \2 \3 \4 \5 \6 \7})

(defn octal-to-binary
  "Convert octal string OCTAL-NUMBER to a binary digit string."
  [octal-number]
  (when (empty? octal-number)
    (throw (ex-info "Empty string was passed to the function"
                    {:type :value-error})))
  (apply str
         (map (fn [digit]
                (when-not (contains? octal-digits digit)
                  (throw (ex-info "Non-octal value was passed to the function"
                                  {:type :value-error})))
                (let [s (Integer/toBinaryString (- (int digit) (int \0)))]
                  (str (apply str (repeat (- 3 (count s)) \0)) s)))
              octal-number)))
