(ns clojure-algorithms.machine-learning.self-organizing-map-test
  (:require [clojure-algorithms.machine-learning.self-organizing-map :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest test-fn-get-winner-examples
  (let [som (sut/make-SelfOrganizingMap)]
    (is (= 1 (sut/SelfOrganizingMap-get-winner som [[1 2 3] [4 5 6]] [1 2 3])))
    (testing "only the first component decides (upstream quirk)"
      (is (= 0 (sut/SelfOrganizingMap-get-winner som [[9 0 0] [0 0 0]] [0 5 5])))
      (is (= 0 (sut/SelfOrganizingMap-get-winner som [[1] [2]] []))))))

(deftest test-fn-update-examples
  (let [som (sut/make-SelfOrganizingMap)]
    (is (approx [[1 2 3] [3.7 4.7 6]]
                (sut/SelfOrganizingMap-update som [[1 2 3] [4 5 6]] [1 2 3]
                                              1 0.1)))
    (testing "untouched row is unchanged and update is pure"
      (let [w [[1 2 3] [4 5 6]]]
        (sut/SelfOrganizingMap-update som w [1 2 3] 1 0.1)
        (is (= [[1 2 3] [4 5 6]] w))))))

(deftest test-fn-main-smoke
  (let [out (with-out-str (sut/main))]
    (is (str/includes? out "Clusters that the test sample belongs to"))
    (is (str/includes? out "Weights that have been trained"))))
