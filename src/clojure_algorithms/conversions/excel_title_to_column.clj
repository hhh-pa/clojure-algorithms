(ns clojure-algorithms.conversions.excel-title-to-column
  "Port of conversions/excel_title_to_column.py.
  Converts an Excel column title to its column number."
  (:require [clojure.string :as str]))

(defn excel-title-to-column
  "Convert Excel COLUMN-TITLE (e.g. \"AB\") to its 1-based column number."
  [column-title]
  (when-not (and (seq column-title) (re-matches #"[A-Za-z]+" column-title))
    (throw (ex-info "Column title must contain only alphabetic characters."
                    {:type :value-error})))
  (reduce (fn [acc [power ch]]
            (+ acc (* (- (int ch) 64) (long (Math/pow 26 power)))))
          0 (map-indexed vector (reverse (str/upper-case column-title)))))
