(ns clojure-algorithms.linear-programming.simplex-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.linear-programming.simplex :as sut]))

(deftest test-fn-init-examples
  (is (thrown? Exception (sut/make-Tableau [[-1 -1 0 0 1]
                                            [1 3 1 0 4]
                                            [3 1 0 1 4]]
                                           2 2)))
  (is (thrown? Exception (sut/make-Tableau [[-1 -1 0 0 -1]
                                            [1 3 1 0 4]
                                            [3 1 0 1 4.0]]
                                           2 2)))
  (is (thrown? Exception (sut/make-Tableau [[-1 -1 0 0 1]
                                            [1 3 1 0 4]
                                            [3 1 0 1 4.0]]
                                           -2 2))))

(deftest test-fn-generate-col-titles-examples
  (is (= ["x1" "x2" "s1" "s2" "RHS"]
         (sut/Tableau-generate-col-titles
          (sut/make-Tableau [[-1 -1 0 0 1]
                             [1 3 1 0 4]
                             [3 1 0 1 4.0]]
                            2 0))))
  (is (= ["x1" "x2" "RHS"]
         (sut/Tableau-generate-col-titles
          (sut/make-Tableau [[-1 -1 0 0 1]
                             [1 3 1 0 4]
                             [3 1 0 1 4.0]]
                            2 2)))))

(deftest test-fn-find-pivot-examples
  (is (= [1 0]
         (sut/Tableau-find-pivot
          (sut/make-Tableau [[-2 1 0 0 0]
                             [3 1 1 0 6]
                             [1 2 0 1 7.0]]
                            2 0)))))

(deftest test-fn-pivot-examples
  (is (approx [[0.0 3.0 2.0 0.0 8.0]
               [1.0 3.0 1.0 0.0 4.0]
               [0.0 -8.0 -3.0 1.0 -8.0]]
              (sut/Tableau-pivot
               (sut/make-Tableau [[-2 -3 0 0 0]
                                  [1 3 1 0 4]
                                  [3 1 0 1 4.0]]
                                 2 2)
               1 0))))

(deftest test-fn-change-stage-examples
  (is (approx [[2.0 1.0 0.0 0.0 0.0]
               [1.0 2.0 -1.0 0.0 2.0]
               [2.0 1.0 0.0 -1.0 2.0]]
              (sut/Tableau-change-stage
               (sut/make-Tableau [[3 3 -1 -1 0 0 4]
                                  [2 1 0 0 0 0 0.0]
                                  [1 2 -1 0 1 0 2]
                                  [2 1 0 -1 0 1 2]]
                                 2 2)))))

(deftest test-fn-run-simplex-standard
  (is (approx {"P" 2.0 "x1" 1.0 "x2" 1.0}
              (sut/Tableau-run-simplex
               (sut/make-Tableau [[-1 -1 0 0 0]
                                  [1 3 1 0 4]
                                  [3 1 0 1 4.0]]
                                 2 0)))))

(deftest test-fn-run-simplex-three-vars
  (is (approx {"P" 5.4 "x1" 0.2 "x3" 1.6}
              (sut/Tableau-run-simplex
               (sut/make-Tableau [[-3 -1 -3 0 0 0 0]
                                  [2 1 1 1 0 0 2]
                                  [1 2 3 0 1 0 5]
                                  [2 2 1 0 0 1 6.0]]
                                 3 0)))))

(deftest test-fn-run-simplex-optimal-input
  (is (approx {"P" 2.0 "x1" 1.0 "x2" 1.0}
              (sut/Tableau-run-simplex
               (sut/make-Tableau [[0 0 0.25 0.25 2]
                                  [0 1 0.375 -0.125 1]
                                  [1 0 -0.125 0.375 1]]
                                 2 0)))))

(deftest test-fn-run-simplex-nonstandard
  (is (approx {"P" 70.0 "x1" 10.0 "x2" 10.0 "x3" 20.0}
              (sut/Tableau-run-simplex
               (sut/make-Tableau [[2 0 0 0 -1 -1 0 0 20]
                                  [-2 -3 -1 0 0 0 0 0 0]
                                  [1 1 1 1 0 0 0 0 40]
                                  [2 1 -1 0 -1 0 1 0 10]
                                  [0 -1 1 0 0 -1 0 1 10.0]]
                                 3 2))))
  (is (approx {"P" 7.0 "x1" 5.0 "x2" 2.0}
              (sut/Tableau-run-simplex
               (sut/make-Tableau [[8 6 0 0 52]
                                  [1 1 0 0 0]
                                  [2 1 1 0 12]
                                  [6 5 0 1 40.0]]
                                 2 2)))))

(deftest test-fn-run-simplex-slack-pivot
  (is (approx {"P" 132.0 "x1" 12.0 "x2" 6.0}
              (sut/Tableau-run-simplex
               (sut/make-Tableau [[2 1 0 0 0 -1 -1 0 0 12.0]
                                  [-8 -6 0 0 0 0 0 0 0 0.0]
                                  [1 3 1 0 0 0 0 0 0 33.0]
                                  [4 2 0 1 0 0 0 0 0 60.0]
                                  [2 4 0 0 1 0 0 0 0 48.0]
                                  [1 1 0 0 0 -1 0 1 0 10.0]
                                  [1 0 0 0 0 0 -1 0 1 2.0]]
                                 2 2)))))

(deftest test-fn-run-simplex-maxiter
  (is (thrown? Exception
               (binding [sut/Tableau-maxiter 0]
                 (sut/Tableau-run-simplex
                  (sut/make-Tableau [[-1 -1 0 0 0]
                                     [1 3 1 0 4]
                                     [3 1 0 1 4.0]]
                                    2 0))))))

(deftest test-fn-interpret-tableau-examples
  (is (approx {"P" 5.0 "x1" 1.0 "x2" 1.0}
              (sut/Tableau-interpret-tableau
               (sut/make-Tableau [[0 0 0.875 0.375 5]
                                  [0 1 0.375 -0.125 1]
                                  [1 0 -0.125 0.375 1]]
                                 2 0)))))
