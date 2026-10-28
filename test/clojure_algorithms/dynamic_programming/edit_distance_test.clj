(ns clojure-algorithms.dynamic-programming.edit-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.edit-distance :as ed]))

(deftest edit-distance-test
  (testing "top-down"
    (is (= 5 (ed/min-dist-top-down "intention" "execution")))
    (is (= 9 (ed/min-dist-top-down "intention" "")))
    (is (= 0 (ed/min-dist-top-down "" ""))))
  (testing "bottom-up"
    (is (= 5 (ed/min-dist-bottom-up "intention" "execution")))
    (is (= 9 (ed/min-dist-bottom-up "intention" "")))
    (is (= 0 (ed/min-dist-bottom-up "" ""))))
  (testing "both variants agree"
    (doseq [[a b] [["kitten" "sitting"] ["sunday" "saturday"] ["abc" "abc"]
                   ["" "abc"] ["a" ""]]]
      (is (= (ed/min-dist-top-down a b) (ed/min-dist-bottom-up a b))))))
