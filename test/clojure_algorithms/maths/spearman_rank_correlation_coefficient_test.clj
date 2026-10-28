(ns clojure-algorithms.maths.spearman-rank-correlation-coefficient-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.spearman-rank-correlation-coefficient :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-assign-ranks-examples
  (is (approx [3.0 1.0 4.0 2.0 5.0] (sut/assign-ranks [3.2 1.5 4.0 2.7 5.1])))
  (is (approx [3.0 1.0 5.0 2.0 4.0] (sut/assign-ranks [10.5 8.1 12.4 9.3 11.0])))
  (is (approx [2.5 2.5 2.5 2.5] (sut/assign-ranks [1 1 1 1]))))

(deftest test-fn-calculate-spearman-rank-correlation-examples
  (is (approx -1.0 (let [x [1 2 3 4 5] y [5 4 3 2 1]]
                     (sut/calculate-spearman-rank-correlation x y))))
  (is (approx 1.0 (let [x [1 2 3 4 5] y [2 4 6 8 10]]
                    (sut/calculate-spearman-rank-correlation x y))))
  (is (approx 0.4 (let [x [1 2 3 4 5] y [5 1 2 9 5]]
                    (sut/calculate-spearman-rank-correlation x y))))
  (is (thrown? Exception (let [x [5] y [9]]
                           (sut/calculate-spearman-rank-correlation x y))))
  (is (= :ValueError (err-type #(sut/calculate-spearman-rank-correlation [5] [9])))))

(deftest test-spearman-extra
  (is (approx [1.0 2.0 3.0] (sut/assign-ranks [10 20 30])))
  (is (approx [1.5 1.5 3.0] (sut/assign-ranks [5 5 9])))
  (is (approx 1.0 (sut/calculate-spearman-rank-correlation [1 2 3] [1 2 3]))))
