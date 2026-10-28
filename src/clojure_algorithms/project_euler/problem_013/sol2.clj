(ns clojure-algorithms.project-euler.problem-013.sol2
  "Project Euler Problem 13: large sum.

  Work out the first ten digits of the sum of one-hundred 50-digit
  numbers.

  This solution adds the numbers column by column, from right to left,
  propagating carries."
  (:require [clojure.string :as str]))

(defn- digit-at
  "Returns the digit value of the character at index d of the
  left-padded number string, treating padding as 0."
  [^String padded d]
  (let [ch (.charAt padded d)]
    (if (= ch \space) 0 (Character/digit ch 10))))

(defn solution
  "Returns the first n digits of the sum of the numbers in content,
  where content is the text of the numbers file (one number per line).
  n should be larger than 2."
  ([content] (solution content 10))
  ([content n]
   (let [numbers (str/split-lines content)
         width (apply max (map count numbers))
         padded (map #(format (str "%" width "s") %) numbers)
         ans (long-array width 0)]
     (doseq [d (range (dec width) -1 -1)]
       (doseq [num padded]
         (aset ans d (+ (aget ans d) (digit-at num d))))
       (when (> d 0)
         (aset ans (dec d) (quot (aget ans d) 10))
         (aset ans d (mod (aget ans d) 10))))
     (let [first-len (count (str (aget ans 0)))
           take-n (min (- (inc n) first-len) width)]
       (apply str (map #(aget ans %) (range take-n)))))))
