(ns clojure-algorithms.project-euler.problem-076.sol1
  "Project Euler 76: counting summations.

  It is possible to write five as a sum in exactly six different ways.
  How many different ways can one hundred be written as a sum of at
  least two positive integers?")

(defn solution
  "Returns the number of different ways the number m can be written as
  a sum of at least two positive integers."
  ([] (solution 100))
  ([m]
   (let [memo (long-array (* (inc m) m))]
     (doseq [i (range (inc m))]
       (aset memo (* i m) 1))
     (doseq [n (range (inc m)) k (range 1 m)]
       (aset memo (+ (* n m) k)
             (+ (aget memo (+ (* n m) k))
                (aget memo (+ (* n m) (dec k)))))
       (when (> n k)
         (aset memo (+ (* n m) k)
               (+ (aget memo (+ (* n m) k))
                  (aget memo (+ (* (- n k 1) m) k))))))
     (dec (aget memo (+ (* m m) (dec m)))))))
