(ns clojure-algorithms.project-euler.problem-022.sol1
  "Project Euler Problem 22: name scores.

  Using the names file, a 46K text file containing over five-thousand
  first names, begin by sorting it into alphabetical order. Then working
  out the alphabetical value for each name, multiply this value by its
  alphabetical position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN,
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
  list. So, COLIN would obtain a score of 938 x 53 = 49714.

  What is the total of all the name scores in the file?"
  (:require [clojure.string :as str]))

(defn solution
  "Returns the total of all the name scores in content, where content is
  the text of the names file (a single line of comma-separated quoted
  names)."
  [content]
  (let [names (sort (str/split (str/replace (first (str/split-lines content))
                                            "\"" "")
                               #","))]
    (reduce + 0
            (map-indexed (fn [idx name]
                           (* (inc idx)
                              (reduce + 0 (map #(- (int %) 64) name))))
                         names))))
