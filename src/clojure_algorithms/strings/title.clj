(ns clojure-algorithms.strings.title
  "Title-case conversion (ASCII). Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn to-title-case
  "Capitalizes the first letter of `word`, lower-casing the rest."
  [word]
  (if (empty? word)
    ""
    (let [first-ch (first word)
          head (if (<= (int \a) (int first-ch) (int \z))
                 (char (- (int first-ch) 32))
                 first-ch)]
      (apply str head (map (fn [ch]
                             (if (<= (int \A) (int ch) (int \Z))
                               (char (+ (int ch) 32))
                               ch))
                           (rest word))))))

(defn sentence-to-title-case
  "Title-cases every word of `s`."
  [s]
  (str/join " " (map to-title-case (str/split s #"\s+"))))
