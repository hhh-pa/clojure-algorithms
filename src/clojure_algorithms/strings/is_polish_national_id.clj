(ns clojure-algorithms.strings.is-polish-national-id
  "Validation of the Polish PESEL national id.
  Ported from TheAlgorithms/Python.")

(def ^:private multipliers [1 3 7 9 1 3 7 9 1 3])

(defn is-polish-national-id
  "True when `s` is a valid PESEL number. Throws IllegalArgumentException
  for non-string or non-numeric input."
  [s]
  (when-not (string? s)
    (throw (IllegalArgumentException.
            (str "Expected str as input, found " (type s)))))
  (let [n (try (Long/parseLong s)
               (catch NumberFormatException _
                 (throw (IllegalArgumentException.
                         "Expected number as input"))))]
    (if (not (<= 10100000 n 99923199999))
      false
      (let [month (Integer/parseInt (subs s 2 4))
            month-ok? (or (<= 1 month 12) (<= 21 month 32) (<= 41 month 52)
                          (<= 61 month 72) (<= 81 month 92))]
        (if-not month-ok?
          false
          (let [day (Integer/parseInt (subs s 4 6))]
            (if-not (<= 1 day 31)
              false
              (let [digits (butlast s)
                    subtotal (reduce + (map (fn [d m]
                                              (mod (* (Character/digit ^char d 10)
                                                      m)
                                                   10))
                                            digits multipliers))
                    checksum (- 10 (mod subtotal 10))]
                (= checksum (mod n 10))))))))))
