(ns clojure-algorithms.data-structures.arrays.dynamic-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.dynamic-array
             :refer [make-dynamic-array darray-append darray-resize darray-get
                     darray-set darray-len darray-items]]))

(deftest test-dynamic-array
  (let [arr (-> (make-dynamic-array)
                (darray-append 1) (darray-append 2) (darray-append 3))]
    (is (= [1 2 3] (darray-items arr)))
    (is (= [1 2 3 4] (darray-items (darray-append arr 4))))
    (is (= [1 2 3 4 5] (darray-items (-> arr (darray-append 4) (darray-append 5))))))
  (let [arr (-> (make-dynamic-array) (darray-append 1) (darray-append 2)
                (darray-resize 10))]
    (is (= 10 (:capacity arr)))
    (is (= [1 2] (darray-items arr))))
  (let [arr (-> (make-dynamic-array) (darray-append 1) (darray-append 2))]
    (is (= 1 (darray-get arr 0)))
    (is (= 2 (darray-get arr 1)))
    (is (thrown? clojure.lang.ExceptionInfo (darray-get arr 2)))
    (is (thrown? clojure.lang.ExceptionInfo (darray-get arr -1)))
    (is (= [9 2] (darray-items (darray-set arr 0 9))))
    (is (thrown? clojure.lang.ExceptionInfo (darray-set arr 5 9))))
  (let [arr (darray-append (make-dynamic-array) 1)]
    (is (= 1 (darray-len arr)))
    (is (= 2 (darray-len (darray-append arr 2))))))
