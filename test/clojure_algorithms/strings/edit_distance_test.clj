(ns clojure-algorithms.strings.edit-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.edit-distance :refer [edit-distance]]))

(deftest test-edit-distance
  (testing "doctest examples"
    (is (= 1 (edit-distance "GATTIC" "GALTIC")))
    (is (= 2 (edit-distance "NUM3" "HUM2")))
    (is (= 3 (edit-distance "cap" "CAP")))
    (is (= 3 (edit-distance "Cat" "")))
    (is (= 0 (edit-distance "cat" "cat")))
    (is (= 9 (edit-distance "" "123456789")))
    (is (= 5 (edit-distance "Be@uty" "Beautyyyy!")))
    (is (= 1 (edit-distance "lstring" "lsstring"))))
  (testing "main example"
    (is (= 4 (edit-distance "ATCGCTG" "TAGCTAA")))))
