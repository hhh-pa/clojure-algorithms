(ns clojure-algorithms.conversions.convert-number-to-words
  "Port of conversions/convert_number_to_words.py.
  Converts integers to English words (short, long, Indian systems)."
  (:require [clojure.string :as str]))

(def numbering-systems
  "Numbering system name to vector of [power unit-word] pairs."
  {"short" [[15 "quadrillion"] [12 "trillion"] [9 "billion"] [6 "million"]
            [3 "thousand"] [2 "hundred"]]
   "long" [[15 "billiard"] [9 "milliard"] [6 "million"] [3 "thousand"]
           [2 "hundred"]]
   "indian" [[14 "crore crore"] [12 "lakh crore"] [7 "crore"] [5 "lakh"]
             [3 "thousand"] [2 "hundred"]]})

(def max-values
  "Numbering system name to largest supported value."
  {"short" (dec (reduce * 1 (repeat 18 10N)))
   "long" (dec (reduce * 1 (repeat 21 10N)))
   "indian" (dec (reduce * 1 (repeat 19 10N)))})

(def ^:private ones
  {0 "" 1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven"
   8 "eight" 9 "nine"})

(def ^:private teens
  {0 "ten" 1 "eleven" 2 "twelve" 3 "thirteen" 4 "fourteen" 5 "fifteen"
   6 "sixteen" 7 "seventeen" 8 "eighteen" 9 "nineteen"})

(def ^:private tens
  {2 "twenty" 3 "thirty" 4 "forty" 5 "fifty" 6 "sixty" 7 "seventy"
   8 "eighty" 9 "ninety"})

(defn max-value
  "Largest value supported by numbering SYSTEM."
  [system]
  (if-let [m (get max-values system)]
    m
    (throw (ex-info "Invalid numbering system" {:type :value-error}))))

(defn convert-small-number
  "Convert a non-negative integer under 100 to words."
  [num]
  (when (neg? num)
    (throw (ex-info "This function only accepts non-negative integers"
                    {:type :value-error})))
  (when (>= num 100)
    (throw (ex-info "This function only converts numbers less than 100"
                    {:type :value-error})))
  (let [[t o] [(quot num 10) (rem num 10)]]
    (cond
      (zero? t) (let [w (ones o)] (if (empty? w) "zero" w))
      (= t 1) (teens o)
      :else (str (tens t) (when-not (empty? (ones o)) "-") (ones o)))))

(declare convert-number)

(defn convert-number
  "Convert integer NUM to English words using SYSTEM
  (\"short\", \"long\" or \"indian\")."
  ([num] (convert-number num "short"))
  ([num system]
   (let [prefix (when (neg? num) ["negative"])
         n (if (neg? num) (- num) num)]
     (when (> n (max-value system))
       (throw (ex-info "Input number is too large" {:type :value-error})))
     (let [[groups rest-n]
           (reduce (fn [[gs r] [power unit]]
                     (let [[digit new-r] [(quot r (long (Math/pow 10 power)))
                                          (rem r (long (Math/pow 10 power)))]]
                       (if (pos? digit)
                         [(conj gs (str (if (>= digit 100)
                                          (convert-number digit system)
                                          (convert-small-number digit))
                                        " " unit))
                          new-r]
                         [gs new-r])))
                   [[] n] (numbering-systems system))]
       (str/join " " (concat prefix groups
                             (when (or (pos? rest-n) (empty? groups))
                               [(convert-small-number rest-n)])))))))
