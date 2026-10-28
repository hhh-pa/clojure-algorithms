(ns clojure-algorithms.strings.upper
  "ASCII-only uppercase conversion. Ported from TheAlgorithms/Python.")

(defn upper
  "Upper-cases ASCII lowercase letters in `s`, leaving the rest untouched."
  [s]
  (apply str (map (fn [ch]
                    (if (<= (int \a) (int ch) (int \z))
                      (char (- (int ch) 32))
                      ch))
                  s)))
