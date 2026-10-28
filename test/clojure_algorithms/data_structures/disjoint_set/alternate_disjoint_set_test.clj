(ns clojure-algorithms.data-structures.disjoint-set.alternate-disjoint-set-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.disjoint-set.alternate-disjoint-set
             :refer [make-disjoint-set get-parent ds-merge]]))

(deftest test-merge
  (let [[a r1] (ds-merge (make-disjoint-set [1 1 1]) 1 2)
        [a r2] (ds-merge a 0 2)
        [_ r3] (ds-merge a 0 1)]
    (is (true? r1))
    (is (true? r2))
    (is (false? r3))))

(deftest test-get-parent
  (let [[a _] (ds-merge (make-disjoint-set [1 1 1]) 1 2)
        [a p0] (get-parent a 0)
        [_ p1] (get-parent a 1)]
    (is (= 0 p0))
    (is (= 2 p1))
    (is (= 2 (:max-set a)))))
