(ns clojure-algorithms.hashes.sdbm-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.sdbm :refer [sdbm]]))

(deftest test-sdbm
  (is (= 1462174910723540325254304520539387479031000036N (bigint (sdbm "Algorithms"))))
  (is (= 730247649148944819640658295400555317318720608290373040936089N
         (bigint (sdbm "scramble bits"))))
  (is (= 0 (sdbm ""))))
