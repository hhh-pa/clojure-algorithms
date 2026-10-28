(ns clojure-algorithms.machine-learning.astar-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.astar :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/astar))
  (is (some? #'sut/make-Cell))
  (is (some? #'sut/Cell-eq))
  (is (some? #'sut/Cell-showcell))
  (is (some? #'sut/make-Gridworld))
  (is (some? #'sut/Gridworld-show))
  (is (some? #'sut/Gridworld-get-neighbours)))
