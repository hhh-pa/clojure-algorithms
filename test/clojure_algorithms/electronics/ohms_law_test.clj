(ns clojure-algorithms.electronics.ohms-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.ohms-law :as ol]))

(deftest ohms-law-test
  (testing "solved quantities"
    (is (= {"current" 2.0} (ol/ohms-law 10 0 5)))
    (is (= {"resistance" -10.0} (ol/ohms-law -10 1 0)))
    (is (= {"voltage" -3.0} (ol/ohms-law 0 -1.5 2))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (ol/ohms-law 0 0 10)))
    (is (thrown? IllegalArgumentException (ol/ohms-law 0 1 -2)))))
