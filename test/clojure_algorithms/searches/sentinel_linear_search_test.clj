(ns clojure-algorithms.searches.sentinel-linear-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.sentinel-linear-search :as sls]))

(deftest sentinel-linear-search-test
  (testing "sentinel search hits and misses"
    (is (= 0 (sls/sentinel-linear-search [0 5 7 10 15] 0)))
    (is (= 4 (sls/sentinel-linear-search [0 5 7 10 15] 15)))
    (is (= 1 (sls/sentinel-linear-search [0 5 7 10 15] 5)))
    (is (nil? (sls/sentinel-linear-search [0 5 7 10 15] 6)))))
