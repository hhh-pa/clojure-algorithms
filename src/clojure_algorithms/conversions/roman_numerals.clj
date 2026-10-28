(ns clojure-algorithms.conversions.roman-numerals
  "Port of conversions/roman_numerals.py.
  Converts between Roman numerals and integers."
  (:require [clojure.string :as str]))

(def roman-table
  "Arabic value to Roman numeral pairs, largest first."
  [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"]
   [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]])

(def ^:private roman-vals
  {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn roman-to-int
  "Convert Roman numeral string ROMAN to an integer."
  [roman]
  (loop [place 0 total 0]
    (if (>= place (count roman))
      total
      (let [cur (roman-vals (nth roman place))
            nxt (when (< (inc place) (count roman))
                  (roman-vals (nth roman (inc place))))]
        (if (and nxt (< cur nxt))
          (recur (+ place 2) (+ total (- nxt cur)))
          (recur (inc place) (+ total cur)))))))

(defn int-to-roman
  "Convert integer NUMBER (1-3999) to a Roman numeral string."
  [number]
  (when (or (boolean? number) (not (integer? number)))
    (throw (ex-info (str "int_to_roman only accepts integers, got "
                         (cond (boolean? number) "bool"
                               (float? number) "float"
                               :else "str"))
                    {:type :type-error})))
  (when-not (<= 1 number 3999)
    (throw (ex-info (str "int_to_roman only accepts integers in the range "
                         "1 to 3999, got " number)
                    {:type :value-error})))
  (loop [n number table roman-table acc ""]
    (if (or (zero? n) (empty? table))
      acc
      (let [[arabic sym] (first table)
            factor (quot n arabic)]
        (recur (rem n arabic) (rest table)
               (str acc (str/join (repeat factor sym))))))))
