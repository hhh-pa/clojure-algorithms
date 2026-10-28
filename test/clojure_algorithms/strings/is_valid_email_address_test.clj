(ns clojure-algorithms.strings.is-valid-email-address-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-valid-email-address
             :refer [is-valid-email-address email-tests]]))

(deftest test-is-valid-email-address
  (testing "all reference cases"
    (doseq [[email valid] email-tests]
      (is (= valid (is-valid-email-address email)) email))))
