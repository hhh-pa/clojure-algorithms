(ns clojure-algorithms.sorts.natural-sort
  "Natural (human) sort for strings with embedded numbers.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn- alphanum-key
  "Splits `s` into digit and non-digit runs. Numeric runs sort as numbers
  (tagged 0), text runs case-insensitively (tagged 1)."
  [s]
  (mapv (fn [tok]
          (if (re-matches #"\d+" tok)
            [0 (bigint tok) ""]
            [1 nil (str/lower-case tok)]))
        (filter (complement str/blank?)
                (str/split s #"(?<=\D)(?=\d)|(?<=\d)(?=\D)"))))

(defn natural-sort
  "Sorts strings `coll` the way humans expect (\"file2\" before \"file10\").
  Returns a sorted vector."
  [coll]
  (vec (sort-by alphanum-key (vec coll))))
