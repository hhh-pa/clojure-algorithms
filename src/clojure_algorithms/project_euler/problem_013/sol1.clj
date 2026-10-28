(ns clojure-algorithms.project-euler.problem-013.sol1
  "Project Euler Problem 13: large sum.

  Work out the first ten digits of the sum of one-hundred 50-digit
  numbers."
  (:require [clojure.string :as str]))

(defn solution
  "Returns the first ten digits of the sum of the numbers in content,
  where content is the text of the numbers file (one number per line)."
  [content]
  (subs (str (reduce + 0 (map bigint (str/split-lines content)))) 0 10))
