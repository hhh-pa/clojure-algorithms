(ns clojure-algorithms.project-euler.problem-142.sol1
  "Project Euler Problem 142: perfect square collection.

  Finds the smallest x + y + z with integers x > y > z > 0 such that
  x + y, x - y, x + z, x - z, y + z, y - z are all perfect squares.
  Changing variables to a, b, c satisfies three requirements automatically
  and the search iterates over a^2, b^2 and c^2 checking the rest.")

(defn solution
  "Iterates over combinations of a, b, c and saves the minimum sum. For one
  term x = 1 is the solution; for two terms x = 5, y = 4 is the solution."
  ([] (solution 3))
  ([number-of-terms]
   (cond
     (= number-of-terms 1) 1
     (= number-of-terms 2) 9
     :else
     (let [n-max 2500
           squares (vec (map #(* % %) (range (inc n-max))))
           squares-set (set squares)
           min-sum (atom nil)]
       (doseq [a (range 1 (count squares))]
         (let [a-sq (nth squares a)]
           (doseq [b (range 1 (count squares))]
             (let [b-sq (nth squares b)]
               (when (contains? squares-set (+ a-sq b-sq))
                 (doseq [c (range (inc (max a b)) (count squares))]
                   (let [c-sq (nth squares c)]
                     (when (or (nil? @min-sum)
                               (<= (quot (+ a-sq (- b-sq) (* 3 c-sq)) 2)
                                   @min-sum))
                       (when (and (contains? squares-set (- c-sq b-sq))
                                  (contains? squares-set (+ a-sq c-sq)))
                         (let [x2 (+ a-sq b-sq c-sq)
                               y2 (+ a-sq (- b-sq) c-sq)
                               z2 (- c-sq a-sq b-sq)]
                           (when (and (pos? z2)
                                      (even? x2)
                                      (even? y2)
                                      (even? z2))
                             (let [sum-val (quot (+ x2 y2 z2) 2)]
                               (swap! min-sum
                                      (fn [old]
                                        (if (nil? old)
                                          sum-val
                                          (min old sum-val))))))))))))))))
       @min-sum))))
