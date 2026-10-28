(ns clojure-algorithms.linear-programming.simplex-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.linear-programming.simplex :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-Tableau))
  (is (some? #'sut/Tableau-generate-col-titles))
  (is (some? #'sut/Tableau-find-pivot))
  (is (some? #'sut/Tableau-pivot))
  (is (some? #'sut/Tableau-change-stage))
  (is (some? #'sut/Tableau-run-simplex))
  (is (some? #'sut/Tableau-interpret-tableau))
  (is (some? #'sut/Tableau-maxiter)))
