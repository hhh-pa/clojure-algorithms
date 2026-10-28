(ns clojure-algorithms.searches.fibonacci-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.fibonacci-search :as fs]))

(deftest fibonacci-test
  (testing "fibonacci numbers"
    (is (= 0 (fs/fibonacci 0)))
    (is (= 1 (fs/fibonacci 2)))
    (is (= 5 (fs/fibonacci 5)))
    (is (= 610 (fs/fibonacci 15))))
  (testing "invalid arguments throw"
    (is (thrown? clojure.lang.ExceptionInfo (fs/fibonacci "a")))
    (is (thrown? clojure.lang.ExceptionInfo (fs/fibonacci -5)))))

(deftest fibonacci-search-test
  (testing "found and missing elements"
    (is (= 0 (fs/fibonacci-search [4 5 6 7] 4)))
    (is (= -1 (fs/fibonacci-search [4 5 6 7] -10)))
    (is (= 0 (fs/fibonacci-search [-18 2] -18)))
    (is (= 0 (fs/fibonacci-search [5] 5)))
    (is (= 1 (fs/fibonacci-search ["a" "c" "d"] "c")))
    (is (= -1 (fs/fibonacci-search ["a" "c" "d"] "f")))
    (is (= -1 (fs/fibonacci-search [] 1)))
    (is (= 1 (fs/fibonacci-search [0.1 0.4 7] 0.4)))
    (is (= 63 (fs/fibonacci-search (vec (range 100)) 63)))
    (is (= 99 (fs/fibonacci-search (vec (range 100)) 99)))
    (is (= 1 (fs/fibonacci-search (vec (range -100 100 3)) -97)))
    (is (= -1 (fs/fibonacci-search (vec (range -100 100 3)) 0)))
    (is (= 20 (fs/fibonacci-search (vec (range -100 100 5)) 0)))
    (is (= 39 (fs/fibonacci-search (vec (range -100 100 5)) 95)))))
