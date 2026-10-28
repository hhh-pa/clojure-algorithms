(ns clojure-algorithms.graphs.a-star-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.a-star :refer [search]]))

(def grid [[0 1 0 0 0 0] [0 1 0 0 0 0] [0 1 0 0 0 0]
           [0 1 0 0 1 0] [0 0 0 0 1 0]])
(def goal [4 5])

(def heuristic
  (vec (for [i (range (count grid))]
         (vec (for [j (range (count (first grid)))]
                (if (= 1 (get-in grid [i j]))
                  99
                  (+ (Math/abs (- i (first goal)))
                     (Math/abs (- j (second goal))))))))))

(deftest test-a-star
  (let [[path action] (search grid [0 0] goal 1 heuristic)]
    (is (= [[0 0] [1 0] [2 0] [3 0] [4 0] [4 1] [4 2] [4 3] [3 3]
            [2 3] [2 4] [2 5] [3 5] [4 5]]
           path))
    (is (= [[0 0 0 0 0 0] [2 0 0 0 0 0] [2 0 0 0 3 3]
            [2 0 0 0 0 2] [2 3 3 3 0 2]]
           action)))
  (is (thrown? IllegalArgumentException
               (search [[0 1] [1 0]] [0 0] [1 1] 1 [[2 99] [99 0]]))))
