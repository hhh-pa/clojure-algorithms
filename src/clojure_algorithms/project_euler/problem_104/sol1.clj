(ns clojure-algorithms.project-euler.problem-104.sol1
  "Project Euler 104: pandigital Fibonacci ends.

  Given that Fk is the first Fibonacci number for which the first nine
  digits AND the last nine digits are 1-9 pandigital, find k.

  The search first sieves Fibonacci numbers modulo 1e9 for last-nine
  pandigitality, then checks the full numbers only at those positions.")

(defn check
  "Takes a number and checks if it is pandigital both from the start
  (first nine digits) and from the end (last nine digits)."
  [number]
  (let [check-last (boolean-array 11 false)
        rest-number (loop [n number k 0]
                      (if (= k 9)
                        n
                        (do (aset check-last (int (rem n 10)) true)
                            (recur (quot n 10) (inc k)))))]
    (if (some #(not (aget check-last (inc %))) (range 9))
      false
      (let [s (str rest-number)
            front (parse-long (subs s 0 (min 9 (count s))))
            check-front (boolean-array 11 false)]
        (loop [n front k 0]
          (when (< k 9)
            (aset check-front (int (rem n 10)) true)
            (recur (quot n 10) (inc k))))
        (every? #(aget check-front (inc %)) (range 9))))))

(defn check1
  "Takes a number and checks if its last nine digits are 1-9
  pandigital."
  [number]
  (let [check-last (boolean-array 11 false)]
    (loop [n number k 0]
      (when (< k 9)
        (aset check-last (int (rem n 10)) true)
        (recur (quot n 10) (inc k))))
    (every? #(aget check-last (inc %)) (range 9))))

(defn solution
  "Returns k such that Fk is the least Fibonacci number pandigital from
  both sides."
  []
  (let [m 1000000000
        tocheck (boolean-array 1000000 false)]
    (loop [x 0 a1 1 b1 1]
      (when (< x 1000000)
        (let [c1 (mod (+ a1 b1) m)
              a1 (mod b1 m)
              b1 (mod c1 m)]
          ;; Guard the tail index: x + 3 can exceed the table only where
          ;; the Python original would raise an IndexError.
          (when (and (check1 b1) (< (+ x 3) 1000000))
            (aset tocheck (+ x 3) true))
          (recur (inc x) a1 b1))))
    (loop [x 0 a 1N b 1N]
      (if (>= x 1000000)
        -1
        (let [c (+ a b) a b b c]
          (if (and (< (+ x 3) 1000000)
                   (aget tocheck (+ x 3))
                   (check b))
            (+ x 3)
            (recur (inc x) a b)))))))
