(ns clojure-algorithms.backtracking.word-break
  "Word Break: can the string be segmented into dictionary words?
  See https://en.wikipedia.org/wiki/Word_break_problem")

(defn backtrack
  "Returns true when the suffix of `input-string` from `start` segments
  into words from `word-dict`."
  [input-string word-dict start]
  (if (= start (count input-string))
    true
    (boolean (some (fn [end]
                     (and (contains? word-dict (subs input-string start end))
                          (backtrack input-string word-dict end)))
                   (range (inc start) (inc (count input-string)))))))

(defn word-break
  "Returns true when `input-string` segments into words from `word-dict`."
  [input-string word-dict]
  (backtrack input-string word-dict 0))
