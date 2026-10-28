(ns clojure-algorithms.maths.gamma-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.gamma :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/gamma-iterative))
  (is (some? #'sut/integrand))
  (is (some? #'sut/gamma-recursive)))
