(ns clojure-algorithms.strings.booths-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.booths-algorithm :refer [find-minimal-rotation]]))

(deftest test-find-minimal-rotation
  (testing "doctest examples"
    (is (= "abac" (find-minimal-rotation "baca")))
    (is (= "aaab" (find-minimal-rotation "aaab")))
    (is (= "abcd" (find-minimal-rotation "abcd")))
    (is (= "adcb" (find-minimal-rotation "dcba")))
    (is (= "aaaab" (find-minimal-rotation "aabaa")))
    (is (= "abc" (find-minimal-rotation "bca"))))
  (testing "empty input throws"
    (is (thrown? IllegalArgumentException (find-minimal-rotation ""))))
  (testing "agrees with brute force on random strings"
    (doseq [_ (range 20)]
      (let [s (apply str (repeatedly (inc (rand-int 12))
                                     #(char (+ (int \a) (rand-int 4)))))
            rots (map #(str (subs s %) (subs s 0 %)) (range (count s)))]
        (is (= (first (sort rots)) (find-minimal-rotation s)) s)))))
