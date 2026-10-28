(ns clojure-algorithms.strings.bitap-string-match-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.bitap-string-match :refer [bitap-string-match]]))

(deftest test-bitap-string-match
  (testing "doctest examples"
    (is (= 5 (bitap-string-match "abdabababc" "ababc")))
    (is (= 0 (bitap-string-match "aaaaaaaaaaaaaaaaaa" "a")))
    (is (= 0 (bitap-string-match "zxywsijdfosdfnso" "zxywsijdfosdfnso")))
    (is (= 0 (bitap-string-match "abdabababc" "")))
    (is (= 9 (bitap-string-match "abdabababc" "c")))
    (is (= -1 (bitap-string-match "abdabababc" "fofosdfo")))
    (is (= -1 (bitap-string-match "abdab" "fofosdfo")))))
