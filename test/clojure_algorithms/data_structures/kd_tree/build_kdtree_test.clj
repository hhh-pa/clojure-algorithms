(ns clojure-algorithms.data-structures.kd-tree.build-kdtree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.kd-tree.build-kdtree
             :refer [build-kdtree]]))

(deftest test-build-kdtree
  (is (nil? (build-kdtree [])))
  (let [t (build-kdtree [[1 2] [3 4] [5 6]])]
    (is (= [3 4] (:point t)))
    (is (= [1 2] (:point (:left t))))
    (is (= [5 6] (:point (:right t)))))
  (let [pts (mapv (fn [i] [(* i 1.0) (* i 2.0)]) (range 10))
        t (build-kdtree pts 2)]
    (is (= 2 (count (:point t)))))
  (let [pts (mapv (fn [i] [(* i 1.0) (* i 2.0) (* i 3.0)]) (range 10))
        t (build-kdtree pts -2)]
    (is (= 3 (count (:point t))))))
