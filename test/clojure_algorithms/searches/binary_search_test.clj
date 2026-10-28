(ns clojure-algorithms.searches.binary-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.searches.binary-search :as bs]))

(deftest bisect-test
  (testing "bisect-left"
    (is (= 0 (bs/bisect-left [0 5 7 10 15] 0)))
    (is (= 2 (bs/bisect-left [0 5 7 10 15] 6)))
    (is (= 5 (bs/bisect-left [0 5 7 10 15] 20)))
    (is (= 3 (bs/bisect-left [0 5 7 10 15] 15 1 3)))
    (is (= 2 (bs/bisect-left [0 5 7 10 15] 6 2)))
    (is (= 0 (bs/bisect-left [] 2)))
    (is (= 0 (bs/bisect-left [2 2 2] 2)))
    (is (= 1 (bs/bisect-left [1 2 2 2 4] 2))))
  (testing "bisect-right"
    (is (= 1 (bs/bisect-right [0 5 7 10 15] 0)))
    (is (= 5 (bs/bisect-right [0 5 7 10 15] 15)))
    (is (= 2 (bs/bisect-right [0 5 7 10 15] 6)))
    (is (= 3 (bs/bisect-right [0 5 7 10 15] 15 1 3)))
    (is (= 2 (bs/bisect-right [0 5 7 10 15] 6 2)))
    (is (= 0 (bs/bisect-right [] 2)))
    (is (= 3 (bs/bisect-right [2 2 2] 2)))
    (is (= 4 (bs/bisect-right [1 2 2 2 4] 2)))))

(deftest insort-test
  (testing "insort-left returns a new vector"
    (is (= [0 5 6 7 10 15] (bs/insort-left [0 5 7 10 15] 6)))
    (is (= [0 5 7 10 15 20] (bs/insort-left [0 5 7 10 15] 20)))
    (is (= [0 5 7 15 10 15] (bs/insort-left [0 5 7 10 15] 15 1 3)))
    (is (= [[0 0] [5 5] [5 5] [7 7] [10 10] [15 15]]
           (bs/insort-left [[0 0] [5 5] [7 7] [10 10] [15 15]] [5 5]))))
  (testing "insort-right returns a new vector"
    (is (= [0 5 6 7 10 15] (bs/insort-right [0 5 7 10 15] 6)))
    (is (= [0 5 7 10 15 20] (bs/insort-right [0 5 7 10 15] 20)))
    (is (= [0 5 7 15 10 15] (bs/insort-right [0 5 7 10 15] 15 1 3)))))

(deftest binary-search-test
  (testing "leftmost binary search"
    (is (= 0 (bs/binary-search [0 5 7 10 15] 0)))
    (is (= 4 (bs/binary-search [0 5 7 10 15] 15)))
    (is (= 1 (bs/binary-search [0 5 7 10 15] 5)))
    (is (= -1 (bs/binary-search [0 5 7 10 15] 6)))
    (is (= 2 (bs/binary-search [1 2 4 4 4 6 7] 4)))
    (is (= 3 (bs/binary-search [0 5 7 10 10 10] 10))))
  (testing "stdlib-style binary search"
    (is (= 0 (bs/binary-search-std-lib [0 5 7 10 15] 0)))
    (is (= 4 (bs/binary-search-std-lib [0 5 7 10 15] 15)))
    (is (= 1 (bs/binary-search-std-lib [0 5 7 10 15] 5)))
    (is (= -1 (bs/binary-search-std-lib [0 5 7 10 15] 6))))
  (testing "duplicates variant"
    (is (= [0] (bs/binary-search-with-duplicates [0 5 7 10 15] 0)))
    (is (= [4] (bs/binary-search-with-duplicates [0 5 7 10 15] 15)))
    (is (= [1 2 3] (bs/binary-search-with-duplicates [1 2 2 2 3] 2)))
    (is (= [] (bs/binary-search-with-duplicates [1 2 2 2 3] 4))))
  (testing "recursive binary search"
    (is (= 0 (bs/binary-search-by-recursion [0 5 7 10 15] 0 0 4)))
    (is (= 4 (bs/binary-search-by-recursion [0 5 7 10 15] 15 0 4)))
    (is (= 1 (bs/binary-search-by-recursion [0 5 7 10 15] 5 0 4)))
    (is (= -1 (bs/binary-search-by-recursion [0 5 7 10 15] 6 0 4)))
    (is (= 2 (bs/binary-search-by-recursion [1 2 4 4 4 6 7] 4 0 6)))
    (is (= 3 (bs/binary-search-by-recursion [0 5 7 10 10 10] 10 0 5))))
  (testing "exponential search"
    (is (= 0 (bs/exponential-search [0 5 7 10 15] 0)))
    (is (= 4 (bs/exponential-search [0 5 7 10 15] 15)))
    (is (= 1 (bs/exponential-search [0 5 7 10 15] 5)))
    (is (= -1 (bs/exponential-search [0 5 7 10 15] 6))))
  (testing "unsorted input throws"
    (is (thrown? clojure.lang.ExceptionInfo (bs/binary-search [3 1 2] 1)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (bs/binary-search-with-duplicates [2 1] 1)))))
