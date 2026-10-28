(ns clojure-algorithms.fractals.koch-snowflake-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.koch-snowflake
             :refer [rotate iteration-step iterate-snowflake initial-vectors]]))

(defn- approx= [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

(defn- approx-vec= [a b]
  (and (= (count a) (count b)) (every? true? (map approx= a b))))

(deftest rotate-test
  (is (approx-vec= [0.5 0.8660254] (rotate [1 0] 60)))
  (is (approx-vec= [0.0 1.0] (rotate [1 0] 90))))

(deftest step-test
  (let [result (iteration-step [[0 0] [1 0]])
        expected [[0 0] [0.33333333 0.0] [0.5 0.28867513] [0.66666667 0.0] [1 0]]]
    (is (= 5 (count result)))
    (is (every? true? (map approx-vec= result expected)))))

(deftest iterate-test
  (is (= 5 (count (iterate-snowflake [[0 0] [1 0]] 1))))
  (is (= 17 (count (iterate-snowflake [[0 0] [1 0]] 2))))
  (is (= (iterate-snowflake initial-vectors 1)
         (iteration-step initial-vectors))))
