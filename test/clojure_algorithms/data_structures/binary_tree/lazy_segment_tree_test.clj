(ns clojure-algorithms.data-structures.binary-tree.lazy-segment-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.lazy-segment-tree
             :refer [make-seg-tree left-index right-index seg-build seg-update
                     seg-query seg-values]]))

(def ^:private sample-a [1 2 -4 7 3 -5 6 11 -20 9 14 15 5 2 -8])

(defn- make-sample []
  (seg-build (make-seg-tree 15) 1 1 15 sample-a))

(deftest test-lazy-seg-tree
  (is (= 2 (left-index 1)))
  (is (= 4 (left-index 2)))
  (is (= 24 (left-index 12)))
  (is (= 3 (right-index 1)))
  (is (= 5 (right-index 2)))
  (is (= 25 (right-index 12)))
  (let [st (make-sample)]
    (is (= 7 (seg-query st 1 1 15 4 6)))
    (is (= 14 (seg-query st 1 1 15 7 11)))
    (is (= 15 (seg-query st 1 1 15 7 12))))
  (let [st (make-sample)
        [st _] (seg-update st 1 1 15 1 3 111)]
    (is (= 111 (seg-query st 1 1 15 1 15))))
  (let [st (make-sample)
        [st _] (seg-update st 1 1 15 1 3 111)
        [st _] (seg-update st 1 1 15 7 8 235)]
    (is (= [111 111 111 7 3 -5 235 235 -20 9 14 15 5 2 -8]
           (seg-values st)))))
