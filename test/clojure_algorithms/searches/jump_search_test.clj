(ns clojure-algorithms.searches.jump-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.jump-search :as js]))

(deftest jump-search-test
  (testing "jump search hits and misses"
    (is (= 3 (js/jump-search [0 1 2 3 4 5] 3)))
    (is (= 2 (js/jump-search [-5 -2 -1] -1)))
    (is (= -1 (js/jump-search [0 5 10 20] 8)))
    (is (= 10 (js/jump-search [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610]
                              55)))
    (is (= 4 (js/jump-search ["aa" "bb" "cc" "dd" "ee" "ff"] "ee")))
    (is (= -1 (js/jump-search [] 1)))))
