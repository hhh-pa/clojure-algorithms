(ns clojure-algorithms.dynamic-programming.egg-dropping-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.egg-dropping :as ed]))

(deftest egg-dropping-test
  (testing "egg dropping attempts"
    (is (= 5 (ed/egg-dropping 1 5)))
    (is (= 3 (ed/egg-dropping 2 6)))
    (is (= 4 (ed/egg-dropping 2 10)))
    (is (= 0 (ed/egg-dropping 2 0)))
    (is (= 1 (ed/egg-dropping 2 1)))
    (is (= 4 (ed/egg-dropping 3 14)))))
