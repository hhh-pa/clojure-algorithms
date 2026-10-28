(ns clojure-algorithms.graphics.butterfly-pattern
  "ASCII butterfly pattern generator.")

(defn butterfly-pattern
  "Returns the butterfly pattern of size n as a newline-joined string."
  [n]
  (let [row (fn [i]
              (str (apply str (repeat i "*"))
                   (apply str (repeat (- (* 2 (- n i)) 1) " "))
                   (apply str (repeat i "*"))))]
    (clojure.string/join
     "\n" (concat (map row (range 1 n))
                  [(apply str (repeat (- (* 2 n) 1) "*"))]
                  (map row (range (dec n) 0 -1))))))
