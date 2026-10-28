(ns clojure-algorithms.physics.de-broglie-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.de-broglie :refer [de-broglie-wavelength]]))

(deftest test-de-broglie-wavelength
  (testing "known values"
    (is (= "6.62607015e-10" (de-broglie-wavelength 1e-24)))
    (is (= "4.13566770e+09" (de-broglie-wavelength 1e-24 true)))
    (is (= "1.32521403e-14" (de-broglie-wavelength 5e-20))))
  (testing "non-positive momentum throws"
    (is (thrown? clojure.lang.ExceptionInfo (de-broglie-wavelength 0)))
    (is (thrown? clojure.lang.ExceptionInfo (de-broglie-wavelength -1e-24)))))
