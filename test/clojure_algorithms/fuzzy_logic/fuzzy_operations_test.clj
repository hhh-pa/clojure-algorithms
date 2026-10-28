(ns clojure-algorithms.fuzzy-logic.fuzzy-operations-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fuzzy-logic.fuzzy-operations
             :refer [make-fuzzy-set membership fuzzy-union fuzzy-intersection
                     fuzzy-complement]]))

(defn- approx= [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest membership-test
  (let [sheru (make-fuzzy-set "Sheru" 0.4 1 0.6)]
    (is (approx= 0.16666666666666663 (membership sheru 0.5)))
    (is (= 0.0 (membership sheru 0.6))))
  (let [a (make-fuzzy-set "a" 0.1 0.2 0.3)]
    (is (= 0.0 (membership a 0.09)))
    (is (= 0.0 (membership a 0.1)))
    (is (approx= 0.1 (membership a 0.11)))
    (is (= 0.0 (membership a 0.4))))
  (is (approx= 0.2 (membership (make-fuzzy-set "A" 0 0.5 1) 0.1)))
  (is (approx= 0.8 (membership (make-fuzzy-set "B" 0.2 0.7 1) 0.6))))

(deftest union-test
  (let [u (fuzzy-union (make-fuzzy-set "a" 0.1 0.2 0.3)
                       (make-fuzzy-set "b" 0.4 0.5 0.6))]
    (is (= "a U b" (:name u)))
    (is (approx= 0.1 (:left-boundary u)))
    (is (approx= 0.35 (:peak u)))
    (is (approx= 0.6 (:right-boundary u)))))

(deftest intersection-test
  (let [i (fuzzy-intersection (make-fuzzy-set "a" 0.1 0.2 0.3)
                              (make-fuzzy-set "b" 0.4 0.5 0.6))]
    (is (= "a ∩ b" (:name i)))
    (is (approx= 0.4 (:left-boundary i)))
    (is (approx= 0.3 (:peak i)))
    (is (approx= 0.35 (:right-boundary i)))))

(deftest complement-test
  (let [c (fuzzy-complement (make-fuzzy-set "fuzzy_set" 0.1 0.2 0.3))]
    (is (= "¬fuzzy_set" (:name c)))
    (is (approx= 0.7 (:left-boundary c)))
    (is (approx= 0.9 (:peak c)))
    (is (approx= 0.8 (:right-boundary c)))))

(deftest str-test
  (is (= "Sheru: [0.4, 1, 0.6]"
         (str (make-fuzzy-set "Sheru" 0.4 1 0.6)))))
