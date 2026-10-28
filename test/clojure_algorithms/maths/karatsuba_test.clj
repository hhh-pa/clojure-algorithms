(ns clojure-algorithms.maths.karatsuba-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.karatsuba :as sut]))

(deftest test-fn-karatsuba-examples
  (is (approx (sut/karatsuba 15463 23489) 363210407))
  (is (approx (sut/karatsuba 3 9) 27))
  (is (approx (* 123456 789012) (sut/karatsuba 123456 789012)))
  (is (approx 0 (sut/karatsuba 0 99999))))
