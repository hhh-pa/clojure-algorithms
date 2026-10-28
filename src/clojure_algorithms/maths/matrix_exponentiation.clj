(ns clojure-algorithms.maths.matrix-exponentiation
  "Matrix Exponentiation is a technique to solve linear recurrences in
  logarithmic time.")

(defrecord Matrix [t n])

(defn make-Matrix
  "Build a Matrix: either identical to the nested vector provided, or a zero
  square matrix of the given size."
  [arg]
  (if (sequential? arg)
    (->Matrix (mapv vec arg) (count arg))
    (->Matrix (vec (repeat arg (vec (repeat arg 0)))) arg)))

(defn Matrix-mul
  "Multiply two square matrices."
  [a b]
  (let [n (:n a)
        ta (:t a)
        tb (:t b)]
    (->Matrix (vec (for [i (range n)]
                     (vec (for [j (range n)]
                            (reduce +' (map *' (nth ta i)
                                            (map #(nth % j) tb)))))))
              n)))

(defn modular-exponentiation
  "Raise matrix a to the b-th power by squaring."
  [a b]
  (loop [matrix (make-Matrix [[1 0] [0 1]])
         a a
         b b]
    (if (<= b 0)
      matrix
      (recur (if (odd? b) (Matrix-mul matrix a) matrix)
             (Matrix-mul a a)
             (bit-shift-right b 1)))))

(defn fibonacci-with-matrix-exponentiation
  "Returns the nth number of the Fibonacci sequence starting with f1 and f2,
  using matrix exponentiation."
  [n f1 f2]
  (cond (= n 1) f1
        (= n 2) f2
        :else (let [matrix (modular-exponentiation (make-Matrix [[1 1] [1 0]])
                                                   (- n 2))]
                (+' (*' f2 (get-in (:t matrix) [0 0]))
                    (*' f1 (get-in (:t matrix) [0 1]))))))

(defn simple-fibonacci
  "Returns the nth number of the Fibonacci sequence starting with f1 and f2,
  using the definition."
  [n f1 f2]
  (cond (= n 1) f1
        (= n 2) f2
        :else (loop [n (- n 2)
                     f1 f1
                     f2 f2]
                (if (<= n 0)
                  f2
                  (recur (dec n) f2 (+' f1 f2))))))

(defn matrix-exponentiation-time
  "Benchmark fibonacci-with-matrix-exponentiation over 100 random inputs."
  []
  (let [start (System/nanoTime)]
    (dotimes [_ 100]
      (fibonacci-with-matrix-exponentiation (inc (rand-int 70000)) 1 1))
    (let [exec-time (/ (double (- (System/nanoTime) start)) 1e9)]
      (println "With matrix exponentiation the average execution time is "
               (/ exec-time 100))
      exec-time)))

(defn simple-fibonacci-time
  "Benchmark simple-fibonacci over 100 random inputs."
  []
  (let [start (System/nanoTime)]
    (dotimes [_ 100]
      (simple-fibonacci (inc (rand-int 70000)) 1 1))
    (let [exec-time (/ (double (- (System/nanoTime) start)) 1e9)]
      (println "Without matrix exponentiation the average execution time is "
               (/ exec-time 100))
      exec-time)))

(defn main
  []
  (matrix-exponentiation-time)
  (simple-fibonacci-time)
  nil)
