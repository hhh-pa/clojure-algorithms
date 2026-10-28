(ns clojure-algorithms.strings.is-srilankan-phone-number
  "Sri Lankan mobile phone number validation.
  Ported from TheAlgorithms/Python.")

(def ^:private pattern
  #"^(?:0|94|\+94|0{2}94)7(0|1|2|4|5|6|7|8)(-| |)\d{7}$")

(defn is-sri-lankan-phone-number
  "True when `phone` is a valid Sri Lankan mobile number."
  [phone]
  (boolean (re-find pattern phone)))
