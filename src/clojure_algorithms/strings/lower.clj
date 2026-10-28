(ns clojure-algorithms.strings.lower
  "ASCII-only lowercase conversion. Ported from TheAlgorithms/Python.")

(def ascii-uppercase-start (int \A))
(def ascii-uppercase-end (int \Z))
(def ascii-case-offset (- (int \a) (int \A)))

(defn lower
  "Lower-cases ASCII uppercase letters in `s`, leaving the rest untouched."
  [s]
  (apply str (map (fn [ch]
                    (let [code (int ch)]
                      (if (<= ascii-uppercase-start code ascii-uppercase-end)
                        (char (+ code ascii-case-offset))
                        ch)))
                  s)))
