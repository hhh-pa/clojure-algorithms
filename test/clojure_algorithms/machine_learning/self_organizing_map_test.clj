(ns clojure-algorithms.machine-learning.self-organizing-map-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.self-organizing-map :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main))
  (is (some? #'sut/make-SelfOrganizingMap))
  (is (some? #'sut/SelfOrganizingMap-get-winner))
  (is (some? #'sut/SelfOrganizingMap-update)))
