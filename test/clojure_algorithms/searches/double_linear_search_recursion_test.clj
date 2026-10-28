(ns clojure-algorithms.searches.double-linear-search-recursion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.double-linear-search-recursion :as dlr]))

(deftest search-test
  (testing "recursive two-ended search"
    (is (= 5 (dlr/search (vec (range 11)) 5)))
    (is (= 2 (dlr/search [1 2 4 5 3] 4)))
    (is (= -1 (dlr/search [1 2 4 5 3] 6)))
    (is (= 0 (dlr/search [5] 5)))
    (is (= -1 (dlr/search [] 1)))
    (is (= -1 (dlr/search [1 2] 3))))
  (testing "record elements compare by value"
    (let [people (mapv dlr/->Person ["Al" "Bob" "Carl"])]
      (is (= 1 (dlr/search people (dlr/->Person "Bob"))))
      (is (= -1 (dlr/search people "Bob"))))))
