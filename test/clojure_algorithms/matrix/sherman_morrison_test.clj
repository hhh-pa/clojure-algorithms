(ns clojure-algorithms.matrix.sherman-morrison-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.sherman-morrison :as sut]))

(deftest test-fn-init-examples
  (is (= "Matrix consist of 2 rows and 3 columns\n[1, 1, 1]\n[1, 1, 1]"
         (sut/Matrix-str (sut/make-Matrix 2 3 1))))
  (is (= "Matrix consist of 2 rows and 3 columns\n[0, 0, 0]\n[0, 0, 0]"
         (sut/Matrix-str (sut/make-Matrix 2 3)))))

(deftest test-fn-validate-indices-examples
  (is (= false (sut/Matrix-validate-indices (sut/make-Matrix 2 6 0) [2 7])))
  (is (= true (sut/Matrix-validate-indices (sut/make-Matrix 2 6 0) [0 0]))))

(deftest test-fn-getitem-examples
  (is (approx 7 (sut/Matrix-getitem (sut/make-Matrix 3 2 7) [1 0])))
  (is (thrown? AssertionError
               (sut/Matrix-getitem (sut/make-Matrix 3 2 7) [5 0]))))

(deftest test-fn-setitem-examples
  (is (= "Matrix consist of 2 rows and 3 columns\n[ 1,  1,  1]\n[ 1,  1, 51]"
         (sut/Matrix-str (sut/Matrix-setitem (sut/make-Matrix 2 3 1) [1 2] 51))))
  (is (= 51 (sut/Matrix-getitem (sut/Matrix-setitem (sut/make-Matrix 2 3 1)
                                                    [1 2] 51)
                                [1 2]))))

(deftest test-fn-add-examples
  (is (= "Matrix consist of 2 rows and 1 columns\n[-1]\n[-1]"
         (sut/Matrix-str (sut/Matrix-add (sut/make-Matrix 2 1 -4)
                                         (sut/make-Matrix 2 1 3)))))
  (is (thrown? AssertionError
               (sut/Matrix-add (sut/make-Matrix 2 1 1)
                               (sut/make-Matrix 1 1 1)))))

(deftest test-fn-neg-examples
  (is (= "Matrix consist of 2 rows and 2 columns\n[-3,  2]\n[ 2, -3]"
         (let [a (sut/Matrix-setitem (sut/Matrix-setitem (sut/make-Matrix 2 2 3)
                                                         [0 1] -2)
                                     [1 0] -2)]
           (sut/Matrix-str (sut/Matrix-neg a))))))

(deftest test-fn-sub-examples
  (is (= [[5 5] [5 5]]
         (:array (sut/Matrix-sub (sut/make-Matrix 2 2 7)
                                 (sut/make-Matrix 2 2 2))))))

(deftest test-fn-mul-examples
  (is (= "Matrix consist of 2 rows and 3 columns\n[-2, -2, -6]\n[-2, -2, -6]"
         (let [a (sut/Matrix-setitem (sut/Matrix-setitem (sut/make-Matrix 2 3 1)
                                                         [0 2] 3)
                                     [1 2] 3)]
           (sut/Matrix-str (sut/Matrix-mul a -2)))))
  (is (= [[4 -2 5] [8 -4 10] [-12 6 -15]]
         (let [u (-> (sut/make-Matrix 3 1 0)
                     (sut/Matrix-setitem [0 0] 1)
                     (sut/Matrix-setitem [1 0] 2)
                     (sut/Matrix-setitem [2 0] -3))
               v (-> (sut/make-Matrix 3 1 0)
                     (sut/Matrix-setitem [0 0] 4)
                     (sut/Matrix-setitem [1 0] -2)
                     (sut/Matrix-setitem [2 0] 5))]
           (:array (sut/Matrix-mul u (sut/Matrix-transpose v))))))
  (is (thrown? Exception (sut/Matrix-mul (sut/make-Matrix 2 2 1) "x"))))

(deftest test-fn-transpose-examples
  (is (= "Matrix consist of 3 rows and 2 columns\n[0, 0]\n[0, 1]\n[0, 2]"
         (let [a (reduce (fn [m [r c]] (sut/Matrix-setitem m [r c] (* r c)))
                         (sut/make-Matrix 2 3)
                         (for [r (range 2) c (range 3)] [r c]))]
           (sut/Matrix-str (sut/Matrix-transpose a))))))

(deftest test-fn-sherman-morrison-examples
  (is (approx [[1.2857142857142856 -0.14285714285714285 0.3571428571428571]
               [0.5714285714285714 0.7142857142857143 0.7142857142857142]
               [-0.8571428571428571 0.42857142857142855 -0.0714285714285714]]
              (let [ainv (-> (sut/make-Matrix 3 3 0)
                             (sut/Matrix-setitem [0 0] 1)
                             (sut/Matrix-setitem [1 1] 1)
                             (sut/Matrix-setitem [2 2] 1))
                    u (-> (sut/make-Matrix 3 1 0)
                          (sut/Matrix-setitem [0 0] 1)
                          (sut/Matrix-setitem [1 0] 2)
                          (sut/Matrix-setitem [2 0] -3))
                    v (-> (sut/make-Matrix 3 1 0)
                          (sut/Matrix-setitem [0 0] 4)
                          (sut/Matrix-setitem [1 0] -2)
                          (sut/Matrix-setitem [2 0] 5))]
                (:array (sut/Matrix-sherman-morrison ainv u v))))))

(deftest test-fn-sherman-morrison-singular
  (is (nil? (let [ainv (-> (sut/make-Matrix 2 2 0)
                           (sut/Matrix-setitem [0 0] 1)
                           (sut/Matrix-setitem [1 1] 1))
                  u (sut/Matrix-setitem (sut/make-Matrix 2 1 0) [0 0] 1)
                  v (sut/Matrix-setitem (sut/make-Matrix 2 1 0) [0 0] -1)]
              (sut/Matrix-sherman-morrison ainv u v)))))
