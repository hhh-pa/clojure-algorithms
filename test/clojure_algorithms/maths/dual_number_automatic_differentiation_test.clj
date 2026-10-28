(ns clojure-algorithms.maths.dual-number-automatic-differentiation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.dual-number-automatic-differentiation :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/differentiate))
  (is (some? #'sut/make-Dual))
  (is (some? #'sut/Dual-repr))
  (is (some? #'sut/Dual-reduce))
  (is (some? #'sut/Dual-add))
  (is (some? #'sut/Dual-sub))
  (is (some? #'sut/Dual-mul))
  (is (some? #'sut/Dual-truediv))
  (is (some? #'sut/Dual-floordiv))
  (is (some? #'sut/Dual-pow))
  (is (some? #'sut/Dual---radd--))
  (is (some? #'sut/Dual---rmul--)))
