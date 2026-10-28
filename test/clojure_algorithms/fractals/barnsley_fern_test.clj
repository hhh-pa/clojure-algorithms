(ns clojure-algorithms.fractals.barnsley-fern-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.barnsley-fern
             :refer [transform choose-transformation generate-fern]]))

(deftest transform-test
  (is (= [0.0 0.0] (transform [0.0 0.0] 0)))
  (is (= [0.89 2.41] (transform [1.0 1.0] 1)))
  (is (= [0.54 1.68] (transform [2.0 3.0] 3)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"index must be in range"
                        (transform [0.0 0.0] 4))))

(deftest choose-test
  (is (= 0 (choose-transformation 0.0)))
  (is (= 1 (choose-transformation 0.5)))
  (is (= 2 (choose-transformation 0.9)))
  (is (= 3 (choose-transformation 0.97))))

(deftest generate-test
  (let [points (generate-fern 5 0)]
    (is (= 5 (count points)))
    (is (= [0.0 0.0] (first points))))
  (is (= (generate-fern 50 7) (generate-fern 50 7)))
  (let [cloud (generate-fern 2000 42)]
    (is (every? (fn [[x _]] (<= -2.182 x 2.6558)) cloud))
    (is (every? (fn [[_ y]] (<= 0.0 y 9.9984)) cloud)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"iterations must be positive"
                        (generate-fern 0))))
