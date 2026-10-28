(ns clojure-algorithms.maths.find-max-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.find-max :as sut]))

(deftest test-fn-find-max-iterative-examples
  (doseq [nums [[3 2 1] [-3 -2 -1] [3 -3 0] [3.0 3.1 2.9]]]
    (is (= (apply max nums) (sut/find-max-iterative nums))))
  (is (approx 94 (sut/find-max-iterative [2 4 9 7 19 94 5])))
  (is (thrown? Exception (sut/find-max-iterative []))))

(deftest test-fn-find-max-recursive-examples
  (doseq [nums [[3 2 1] [-3 -2 -1] [3 -3 0] [3.0 3.1 2.9]]]
    (is (= (apply max nums)
           (sut/find-max-recursive nums 0 (dec (count nums))))))
  (let [nums [1 3 5 7 9 2 4 6 8 10]]
    (is (= (apply max nums)
           (sut/find-max-recursive nums 0 (dec (count nums)))))
    (is (= (apply max nums)
           (sut/find-max-recursive nums (- (count nums)) -1)))
    (is (thrown? Exception (sut/find-max-recursive nums 0 (count nums))))
    (is (thrown? Exception
                 (sut/find-max-recursive nums (- (inc (count nums))) -1))))
  (is (thrown? Exception (sut/find-max-recursive [] 0 0))))
