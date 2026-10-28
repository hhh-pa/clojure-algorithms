(ns clojure-algorithms.strings.anagrams-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.anagrams :refer [signature anagram
                                                         build-index]]))

(deftest test-signature
  (is (= "e1s1t2" (signature "test")))
  (is (= " 3a1e1h1i2s3t3" (signature "this is a test")))
  (is (= "a1e1f1i1l1n1s1t2" (signature "finaltest"))))

(deftest test-anagram
  (testing "against the bundled dictionary"
    (is (= ["sett" "stet" "test"] (anagram "test")))
    (is (= [] (anagram "this is a test")))
    (is (= ["final"] (anagram "final"))))
  (testing "against a custom index"
    (let [idx (build-index ["listen" "silent" "enlist" "hello"])]
      (is (= ["enlist" "listen" "silent"]
             (sort (anagram "silent" idx)))))))
