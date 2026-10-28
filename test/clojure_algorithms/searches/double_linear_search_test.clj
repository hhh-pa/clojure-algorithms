(ns clojure-algorithms.searches.double-linear-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.double-linear-search :as dls]))

(deftest double-linear-search-test
  (testing "two-ended scan"
    (is (= 0 (dls/double-linear-search [1 5 5 10] 1)))
    (is (= 1 (dls/double-linear-search [1 5 5 10] 5)))
    (is (= -1 (dls/double-linear-search [1 5 5 10] 100)))
    (is (= 3 (dls/double-linear-search [1 5 5 10] 10)))
    (is (= -1 (dls/double-linear-search [] 1)))))
