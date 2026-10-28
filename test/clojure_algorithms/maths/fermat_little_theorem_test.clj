(ns clojure-algorithms.maths.fermat-little-theorem-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.fermat-little-theorem :as sut]))

(deftest test-fn-binary-exponentiation-examples
  (is (approx 4 (sut/binary-exponentiation 2 10 17)))
  (is (approx 1 (sut/binary-exponentiation 3 0 5)))
  (is (approx 8 (sut/binary-exponentiation 5 3 13))))

(deftest test-fermat-little-theorem-extra
  (is (= 1 (sut/binary-exponentiation 7 0 13)))
  (is (= 7 (sut/binary-exponentiation 7 1 13)))
  (is (= 701 sut/p))
  (is (= 1000000000 sut/a))
  (is (= 10 sut/b))
  (is (true? (= (mod (/ sut/a sut/b) sut/p)
                (mod (* sut/a (sut/binary-exponentiation sut/b (- sut/p 2) sut/p))
                     sut/p)))))
