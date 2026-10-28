(ns clojure-algorithms.matrix.count-islands-in-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.count-islands-in-matrix :as sut]))

(deftest test-fn-init-examples
  (is (= 3 (:ROW (sut/make-Matrix [[true false false false]
                                   [true false true false]
                                   [false false true true]]))))
  (is (= 4 (:COL (sut/make-Matrix [[true false false false]
                                   [true false true false]
                                   [false false true true]]))))
  (is (= [[true false false false]
          [true false true false]
          [false false true true]]
         (:graph (sut/make-Matrix [[true false false false]
                                   [true false true false]
                                   [false false true true]])))))

(deftest test-fn-is-safe-examples
  (is (= true (let [visited [[false false false] [false false false] [false false false]]
                    m (sut/make-Matrix [[true false false] [false false true] [false false true]])]
                (sut/Matrix-is-safe m 0 0 visited))))
  (is (= false (let [visited [[false false false] [false false false] [false false false]]
                     m (sut/make-Matrix [[true false false] [false false true] [false false true]])]
                 (sut/Matrix-is-safe m 0 2 visited))))
  (is (= false (let [visited [[false false false] [false false false] [false false false]]
                     m (sut/make-Matrix [[true false false] [false false true] [false false true]])]
                 (sut/Matrix-is-safe m -1 2 visited))))
  (is (= false (let [visited [[false false false] [false false false] [false false false]]
                     m (sut/make-Matrix [[true false false] [false false true] [false false true]])]
                 (sut/Matrix-is-safe m 1 5 visited)))))

(deftest test-fn-diffs-examples
  (is (= [[true true false] [false true false] [true false true]]
         (let [visited [[false false false] [false false false] [false false false]]
               m (sut/make-Matrix [[true true false] [false true false] [true false true]])]
           (sut/Matrix-diffs m 0 0 visited)))))

(deftest test-fn-count-islands-examples
  (is (= 2 (sut/Matrix-count-islands
            (sut/make-Matrix [[true true false false]
                              [false true false true]
                              [true false false true]]))))
  (is (= 2 (sut/Matrix-count-islands
            (sut/make-Matrix [[true true false]
                              [true false false]
                              [false false true]])))))
