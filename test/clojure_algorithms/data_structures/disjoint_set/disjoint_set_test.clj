(ns clojure-algorithms.data-structures.disjoint-set.disjoint-set-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.disjoint-set.disjoint-set
             :refer [make-set find-set union-set find-python-set]]))

(deftest test-make-find
  (let [ds (make-set {} 1)
        [ds2 node] (find-set ds 1)]
    (is (= 1 (:data node)))
    (is (= 0 (:rank node)))
    (is (= node (get (:nodes ds2) 1)))))

(deftest test-union
  (let [ds (-> {} (make-set 1) (make-set 2) (make-set 3))
        ds (union-set ds 1 2)
        [ds n1] (find-set ds 1)
        [_ n2] (find-set ds 2)]
    (is (= (:data n1) (:data n2)))
    (let [ds (union-set ds 1 3)
          [_ m1] (find-set ds 1)
          [ds m2] (find-set ds 2)
          [_ m3] (find-set ds 3)]
      (is (= (:data m1) (:data m3)))
      (is (= (:data m2) (:data m3))))))

(deftest test-six-vertices
  (let [ds (reduce make-set {} (range 6))
        ds (-> ds (union-set 0 1) (union-set 1 2)
               (union-set 3 4) (union-set 3 5))]
    (doseq [a (range 6) b (range 6)]
      (let [[_ na] (find-set ds a)
            [_ nb] (find-set ds b)
            same? (= (:data na) (:data nb))
            disjoint? (empty? (clojure.set/intersection
                               (find-python-set a) (find-python-set b)))]
        (is (= (not disjoint?) same?))))))

(deftest test-find-python-set
  (is (= #{0 1 2} (find-python-set 1)))
  (is (= #{3 4 5} (find-python-set 4)))
  (is (thrown? clojure.lang.ExceptionInfo (find-python-set 9))))
