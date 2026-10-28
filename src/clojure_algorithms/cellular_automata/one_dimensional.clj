(ns clojure-algorithms.cellular-automata.one-dimensional
  "One-dimensional Wolfram cellular automata. PIL rendering is omitted.")

(def cells
  [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

(defn format-ruleset
  "Pads an integer ruleset to an 8-element digit vector."
  [ruleset]
  (mapv #(Character/digit ^char % 10) (take 8 (format "%08d" ruleset))))

(defn new-generation
  "Computes the next row of cells at index time under rule."
  [cells rule time]
  (let [row (nth cells time) population (count row)]
    (mapv (fn [i]
            (let [l (if (zero? i) 0 (nth row (dec i)))
                  c (nth row i)
                  r (if (= i (dec population)) 0 (nth row (inc i)))
                  situation (- 7 (Integer/parseInt (str l c r) 2))]
              (nth rule situation)))
          (range population))))

(defn evolve
  "Evolves until there are n rows and returns them."
  [cells rule n]
  (loop [cells (vec cells)]
    (if (>= (count cells) n)
      (vec (take n cells))
      (recur (conj cells (new-generation cells rule (dec (count cells))))))))
