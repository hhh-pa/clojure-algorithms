(ns clojure-algorithms.machine-learning.federated-averaging-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.federated-averaging :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/-validate-clients))
  (is (some? #'sut/-normalize-weights))
  (is (some? #'sut/federated-average)))
