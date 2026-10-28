(ns clojure-algorithms.web-programming.get-amazon-product-data-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.get-amazon-product-data
             :refer [search-url product-link compute-discount
                     get-amazon-product-data]]))

(deftest test-pure-logic
  (is (= "https://www.amazon.in/laptop/s?k=headphones"
         (search-url "headphones")))
  (is (= "https://www.amazon.in/dp/ABC" (product-link "dp/ABC")))
  (is (< (abs (- 25.0 (compute-discount "₹1,000" "₹750"))) 1e-9))
  (is (Double/isNaN (compute-discount "" "₹750"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (get-amazon-product-data "headphones"))))
