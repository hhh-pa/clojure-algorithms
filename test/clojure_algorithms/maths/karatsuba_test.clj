(ns clojure-algorithms.maths.karatsuba-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.karatsuba :as sut]))

(deftest test-fn-karatsuba-examples
  (is (approx (sut/karatsuba 15463 23489) 363210407))
  (is (approx (sut/karatsuba 3 9) 27)))
