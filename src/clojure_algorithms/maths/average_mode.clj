(ns clojure-algorithms.maths.average-mode
  "Port of maths/average_mode.py")

(defn mode
  "Returns the mode (Mode as in the measures of central tendency) of the
input data as a sorted vector. The input may contain any datatype."
  [input-list]
  (if (empty? input-list)
    []
    (let [freqs (frequencies input-list)
          best (apply max (vals freqs))]
      (vec (sort (for [[v c] freqs :when (= c best)] v))))))
