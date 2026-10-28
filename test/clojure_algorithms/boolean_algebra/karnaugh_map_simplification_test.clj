(ns clojure-algorithms.boolean-algebra.karnaugh-map-simplification-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.karnaugh-map-simplification :as sut]))

(deftest kmap-test
  (is (= "A'B + AB' + AB" (sut/simplify-kmap [[0 1] [1 1]])))
  (is (= "" (sut/simplify-kmap [[0 0] [0 0]])))
  (is (= "A'B + AB' + AB" (sut/simplify-kmap [[0 1] [1 -1]])))
  (is (= "A'B" (sut/simplify-kmap [[0 1] [0 0]])))
  (is (= "AB" (sut/simplify-kmap [[0 0] [0 1]]))))

(deftest simplify-kmap-truthy-cells-test
  (is (= "A'B + AB' + AB" (sut/simplify-kmap [[0 1] [1 2]])))
  (is (= "A'B + AB' + AB" (sut/simplify-kmap [[0 1] [1 1.1]])))
  (is (= "A'B + AB' + AB" (sut/simplify-kmap [[0 1] [1 "a"]]))))

(deftest main-test
  (is (= "[0, 1]\n[1, 1]\nSimplified Expression:\nA'B + AB' + AB\n"
         (with-out-str (sut/main)))))
