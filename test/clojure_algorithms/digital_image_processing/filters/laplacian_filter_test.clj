(ns clojure-algorithms.digital-image-processing.filters.laplacian-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.laplacian-filter
             :refer [my-laplacian kernels]]))

(deftest test-my-laplacian
  (is (thrown? clojure.lang.ExceptionInfo (my-laplacian [] 0)))
  (is (thrown? clojure.lang.ExceptionInfo (my-laplacian [[1]] 2)))
  (let [img (vec (repeat 5 (vec (repeat 5 10))))]
    (doseq [k [1 3 5 7]]
      (let [out (my-laplacian img k)
            expected (* 10 (reduce + (mapcat identity (kernels k))))]
        (is (= 5 (count out)))
        (is (every? #(= expected %) (mapcat identity out))))))
  (let [out (my-laplacian [[1 2 3] [4 5 6] [7 8 9]] 3)]
    (is (= 3 (count out)))
    (is (= -8 (get-in out [1 1])))))
