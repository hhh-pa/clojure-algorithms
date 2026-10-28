(ns clojure-algorithms.strings.text-justification
  "Full (left and right) text justification.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn- justify-line
  [line width max-width]
  (let [spaces (- max-width width)]
    (if (= 1 (count line))
      (str (first line) (apply str (repeat spaces " ")))
      (let [gaps (dec (count line))
            base (quot spaces gaps)
            extra (mod spaces gaps)
            widths (mapv (fn [i] (+ base (if (< i extra) 1 0)))
                         (range gaps))]
        (apply str (concat (mapcat (fn [w sp]
                                     [w (apply str (repeat sp " "))])
                                   (butlast line) widths)
                           [(last line)]))))))

(defn text-justification
  "Justifies `text` to lines of exactly `max-width` chars.
  Returns a vector of lines (last line left-justified)."
  [text max-width]
  (let [words (str/split text #"\s+")
        [lines cur width]
        (reduce (fn [[ans line w] word]
                  (if (<= (+ w (count word) (count line)) max-width)
                    [ans (conj line word) (+ w (count word))]
                    [(conj ans (justify-line line w max-width))
                     [word] (count word)]))
                [[] [] 0] words)
        remaining (- max-width width (count cur))]
    (conj lines (str (str/join " " cur)
                     (apply str (repeat (inc remaining) " "))))))
