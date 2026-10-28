(ns clojure-algorithms.hashes.jenkins-one-at-a-time-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.jenkins-one-at-a-time :refer [jenkins-one-at-a-time]]))

(deftest test-jenkins-one-at-a-time
  (is (= 2297466611 (jenkins-one-at-a-time "apple")))
  (is (= 1064684737 (jenkins-one-at-a-time "test")))
  (is (= 0 (jenkins-one-at-a-time ""))))
