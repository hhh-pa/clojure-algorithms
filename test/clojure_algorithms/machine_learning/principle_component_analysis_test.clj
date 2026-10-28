(ns clojure-algorithms.machine-learning.principle-component-analysis-test
  (:require [clojure-algorithms.machine-learning.principle-component-analysis
             :as sut]
            [clojure.test :refer [deftest is]]))

(deftest test-sklearn-stubs-throw
  (doseq [[f args] [[sut/collect-dataset []]
                    [sut/apply-pca [nil 2]]
                    [sut/main []]]]
    (is (= :UnsupportedOperation
           (:type (try (apply f args)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
