(ns clojure-algorithms.other.number-container-system-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.number-container-system :as n]))

(deftest binary-search-delete-test
  (is (= [1 3] (n/binary-search-delete [1 2 3] 2)))
  (is (= [0 0] (n/binary-search-delete [0 0 0] 0)))
  (is (= [-1] (n/binary-search-delete [-1 0] 0)))
  (is (= [0] (n/binary-search-delete [-1 0] -1)))
  (is (thrown? IllegalArgumentException
               (n/binary-search-delete [0 -1 2 4] 0))))

(deftest binary-search-insert-test
  (is (= [1 2 2 3] (n/binary-search-insert [1 2 3] 2)))
  (is (= [0 1 2 3] (n/binary-search-insert [0 1 3] 2)))
  (is (= [-5 -3 0 0 11 51 103]
         (n/binary-search-insert [-5 -3 0 0 11 103] 51))))

(deftest change-find-test
  (testing "doctest trace"
    (let [c (n/make-container)]
      (is (= -1 (n/find-smallest c 10)))
      (let [c (n/change c 0 10)]
        (is (= 0 (n/find-smallest c 10)))
        (let [c (n/change c 0 20)]
          (is (= -1 (n/find-smallest c 10)))
          (is (= 0 (n/find-smallest c 20)))))))
  (testing "smallest index wins"
    (let [c (-> (n/make-container)
                (n/change 5 7) (n/change 2 7) (n/change 9 7))]
      (is (= 2 (n/find-smallest c 7))))))
