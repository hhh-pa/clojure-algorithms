(ns clojure-algorithms.dynamic-programming.min-distance-up-bottom-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.min-distance-up-bottom :as md]))

(deftest min-distance-test
  (testing "edit distances"
    (is (= 5 (md/min-distance-up-bottom "intention" "execution")))
    (is (= 9 (md/min-distance-up-bottom "intention" "")))
    (is (= 0 (md/min-distance-up-bottom "" "")))
    (is (= 10 (md/min-distance-up-bottom "zooicoarchaeologist" "zoologist")))
    (is (= 3 (md/min-distance-up-bottom "kitten" "sitting")))))
