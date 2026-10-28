(ns clojure-algorithms.electronics.circular-convolution-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.circular-convolution :as cc]))

(defn- approx-vecs [expected actual]
  (and (= (count expected) (count actual))
       (every? (fn [[e a]] (< (Math/abs (- e a)) 1e-9))
               (map vector expected actual))))

(deftest circular-convolution-test
  (testing "circular convolution via the matrix method"
    (is (approx-vecs [10.0 10.0 6.0 14.0]
                     (cc/circular-convolution [2 1 2 -1] [1 2 3 4])))
    (is (approx-vecs [5.2 6.0 6.48 6.64 6.48 6.0 5.2 4.08]
                     (cc/circular-convolution [0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6]
                                              [0.1 0.3 0.5 0.7 0.9 1.1 1.3 1.5])))
    (is (approx-vecs [6.25 -3.0 1.5 -2.0 -2.75]
                     (cc/circular-convolution [-1 1 2 -2] [0.5 1 -1 2 0.75])))
    (is (approx-vecs [8.0 -2.0 3.0 4.0 11.0]
                     (cc/circular-convolution [1 -1 2 3 -1] [1 2 3])))))
