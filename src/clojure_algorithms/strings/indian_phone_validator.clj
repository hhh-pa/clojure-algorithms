(ns clojure-algorithms.strings.indian-phone-validator
  "Indian phone number validation.
  Ported from TheAlgorithms/Python.")

(def ^:private pattern #"^(\+91[\-\s]?)?[0]?(91)?[789]\d{9}$")

(defn indian-phone-validator
  "True when `phone` is a valid Indian phone number."
  [phone]
  (boolean (re-matches pattern phone)))
