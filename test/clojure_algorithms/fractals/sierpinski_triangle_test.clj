(ns clojure-algorithms.fractals.sierpinski-triangle-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.sierpinski-triangle
             :refer [get-mid triangle]]))

(deftest mid-test
  (is (= [1.0 1.0] (get-mid [0 0] [2 2])))
  (is (= [0.0 0.0] (get-mid [-3 -3] [3 3])))
  (is (= [2.0 1.0] (get-mid [1 0] [3 2])))
  (is (= [0.5 0.5] (get-mid [0 0] [1 1])))
  (is (= [0.0 0.0] (get-mid [0 0] [0 0]))))

(deftest triangle-test
  (let [v1 [-175 -125] v2 [0 175] v3 [175 -125]]
    (is (= 1 (count (triangle v1 v2 v3 0))))
    (is (= [v1 v2 v3] (first (triangle v1 v2 v3 0))))
    (is (= 4 (count (triangle v1 v2 v3 1))))
    (is (= 13 (count (triangle v1 v2 v3 2))))))
