(ns clojure-algorithms.other.least-recently-used-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.least-recently-used :as l]))

(deftest lru-test
  (testing "doctest trace"
    (let [c (-> (l/make-lru 4)
                (l/refer "A") (l/refer 2) (l/refer 3))]
      (is (= [3 2 "A"] (l/cache-keys c))))
    (let [c (-> (l/make-lru 4)
                (l/refer "A") (l/refer 2) (l/refer 3)
                (l/refer "A") (l/refer 4) (l/refer 5))]
      (is (= [5 4 "A" 3] (l/cache-keys c)))))
  (testing "re-refer moves key to front"
    (is (= ["A" 3 2] (l/cache-keys (-> (l/make-lru 4)
                                       (l/refer "A") (l/refer 2)
                                       (l/refer 3) (l/refer "A"))))))
  (testing "negative capacity throws"
    (is (thrown? IllegalArgumentException (l/make-lru -1)))))
