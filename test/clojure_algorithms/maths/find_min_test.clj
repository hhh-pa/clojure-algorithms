(ns clojure-algorithms.maths.find-min-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.find-min :as sut]))

(deftest test-fn-find-min-iterative-examples
  (doseq [nums [[3 2 1] [-3 -2 -1] [3 -3 0] [3.0 3.1 2.9]]]
    (is (approx (reduce min nums) (sut/find-min-iterative nums))))
  (is (approx -56 (sut/find-min-iterative [0 1 2 3 4 5 -3 24 -56])))
  (is (thrown? Exception (sut/find-min-iterative []))))

(deftest test-fn-find-min-recursive-examples
  (doseq [nums [[3 2 1] [-3 -2 -1] [3 -3 0] [3.0 3.1 2.9]]]
    (is (approx (reduce min nums)
                (sut/find-min-recursive nums 0 (dec (count nums))))))
  (let [nums [1 3 5 7 9 2 4 6 8 10]]
    (is (approx (reduce min nums)
                (sut/find-min-recursive nums 0 (dec (count nums)))))
    (is (thrown? Exception (sut/find-min-recursive nums 0 (count nums))))
    (is (approx (reduce min nums)
                (sut/find-min-recursive nums (- (count nums)) -1)))
    (is (thrown? Exception
                 (sut/find-min-recursive nums (- -1 (count nums)) -1))))
  (is (thrown? Exception (sut/find-min-recursive [] 0 0))))
