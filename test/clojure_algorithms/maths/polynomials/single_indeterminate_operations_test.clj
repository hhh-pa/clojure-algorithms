(ns clojure-algorithms.maths.polynomials.single-indeterminate-operations-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.polynomials.single-indeterminate-operations :as sut]))

(deftest test-fn-repr-examples
  (is (some? (let [p (sut/make-Polynomial 2 [1 2 3])] p))))

(deftest test-fn-eq-examples
  (is (let [p (sut/make-Polynomial 2 [1 2 3]) q (sut/make-Polynomial 2 [1 2 3])] (approx p q))))

(deftest test-fn-ne-examples
  (is (let [p (sut/make-Polynomial 2 [1 2 3]) q (sut/make-Polynomial 2 [1 2 3])] (approx p q))))

(deftest test-constructor-validation
  (is (thrown? Exception (sut/make-Polynomial 2 [1 2 3 4])))
  (let [p (sut/make-Polynomial 2 [1 2 3])]
    (is (= 2 (:degree p)))
    (is (= [1 2 3] (:coefficients p)))))

(deftest test-fn-add-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])
        q (sut/make-Polynomial 2 [1 2 3])]
    (is (= "6x^2 + 4x + 2" (sut/Polynomial-str (sut/Polynomial-add p q)))))
  (let [p (sut/make-Polynomial 1 [1 1])
        q (sut/make-Polynomial 2 [1 2 3])]
    (is (= "3x^2 + 3x + 2" (sut/Polynomial-str (sut/Polynomial-add p q))))
    (is (= "3x^2 + 3x + 2" (sut/Polynomial-str (sut/Polynomial-add q p))))))

(deftest test-fn-sub-examples
  (let [p (sut/make-Polynomial 2 [1 2 4])
        q (sut/make-Polynomial 2 [1 2 3])]
    (is (= "1x^2" (sut/Polynomial-str (sut/Polynomial-sub p q))))))

(deftest test-fn-neg-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])]
    (is (= " - 3x^2 - 2x - 1" (sut/Polynomial-str (sut/Polynomial-neg p))))))

(deftest test-fn-mul-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])
        q (sut/make-Polynomial 2 [1 2 3])]
    (is (= "9x^4 + 12x^3 + 10x^2 + 4x + 1"
           (sut/Polynomial-str (sut/Polynomial-mul p q))))))

(deftest test-fn-evaluate-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])]
    (is (= 17 (sut/Polynomial-evaluate p 2)))))

(deftest test-fn-str-repr-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])]
    (is (= "3x^2 + 2x + 1" (sut/Polynomial-str p)))
    (is (= "3x^2 + 2x + 1" (sut/Polynomial-repr p)))
    (is (= "3x^2 + 2x + 1" (with-out-str (print p))))))

(deftest test-fn-derivative-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])]
    (is (= "6x + 2" (sut/Polynomial-str (sut/Polynomial-derivative p))))))

(deftest test-fn-integral-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])]
    (is (= "1.0x^3 + 1.0x^2 + 1.0x"
           (sut/Polynomial-str (sut/Polynomial-integral p))))))

(deftest test-fn-eq-ne-examples
  (let [p (sut/make-Polynomial 2 [1 2 3])
        q (sut/make-Polynomial 2 [1 2 3])
        r (sut/make-Polynomial 2 [1 2 4])
        s (sut/make-Polynomial 1 [2 3])]
    (is (true? (sut/Polynomial-eq p q)))
    (is (false? (sut/Polynomial-eq p r)))
    (is (false? (sut/Polynomial-eq p s)))
    (is (false? (sut/Polynomial-eq p [1 2 3])))
    (is (false? (sut/Polynomial-ne p q)))
    (is (true? (sut/Polynomial-ne p r)))))
