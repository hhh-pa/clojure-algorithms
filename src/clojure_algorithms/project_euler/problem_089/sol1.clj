(ns clojure-algorithms.project-euler.problem-089.sol1
  "Project Euler 89: Roman numerals.

  Find the number of characters saved by writing each of the given Roman
  numerals in their minimal form.

  This port takes the numerals as a string (one per line) instead of
  reading p089_roman.txt, so the pure logic can be tested with tiny
  data."
  (:require [clojure.string :as str]))

(def ^:private symbols
  {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn parse-roman-numerals
  "Converts a string of Roman numerals to an integer."
  [numerals]
  (loop [index 0 total-value 0]
    (if (>= index (dec (count numerals)))
      (+ total-value (symbols (nth numerals index)))
      (let [current-value (symbols (nth numerals index))
            next-value (symbols (nth numerals (inc index)))]
        (recur (inc index)
               (if (< current-value next-value)
                 (- total-value current-value)
                 (+ total-value current-value)))))))

(defn- minimal-digit
  "Renders one decimal digit in minimal Roman form with the given one,
  five, and ten symbols."
  [digit one five ten]
  (cond
    (= digit 9) (str one ten)
    (= digit 4) (str one five)
    (>= digit 5) (str five (apply str (repeat (- digit 5) one)))
    :else (apply str (repeat digit one))))

(defn generate-roman-numerals
  "Generates a string of minimal Roman numerals for a given integer."
  [num]
  (str (apply str (repeat (quot num 1000) \M))
       (minimal-digit (quot (rem num 1000) 100) \C \D \M)
       (minimal-digit (quot (rem num 100) 10) \X \L \C)
       (minimal-digit (rem num 10) \I \V \X)))

(defn solution
  "Calculates the number of characters saved by writing each of the
  Roman numerals in content (one per line) in minimal form."
  [content]
  (reduce + (map (fn [line]
                   (let [original (str/trim line)]
                     (- (count original)
                        (count (generate-roman-numerals
                                (parse-roman-numerals original))))))
                 (str/split-lines content))))
