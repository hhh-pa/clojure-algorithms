(ns clojure-algorithms.data-structures.hashing.quadratic-probing-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.quadratic-probing
             :refer [make-quadratic-probing ht-keys insert-data]]))

(deftest test-quadratic-probing
  (is (= {11 45 14 99 7 24 0 340 5 73 6 90 8 7}
         (ht-keys (reduce insert-data (make-quadratic-probing 7)
                          [90 340 24 45 99 73 7]))))
  (is (= {0 0 7 999 3 111}
         (ht-keys (reduce insert-data (make-quadratic-probing 8)
                          [0 999 111]))))
  (is (= {0 0 4 999 1 111}
         (ht-keys (reduce insert-data (make-quadratic-probing 2)
                          [0 999 111]))))
  (is (= {4 999 1 111}
         (ht-keys (reduce insert-data (make-quadratic-probing 1)
                          [0 999 111])))))
