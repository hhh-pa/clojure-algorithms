(ns clojure-algorithms.data-compression.run-length-encoding-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.run-length-encoding
             :refer [run-length-encode run-length-decode]]))

(deftest test-encode
  (is (= [[\A 4] [\B 3] [\C 2] [\D 1] [\A 2]]
         (run-length-encode "AAAABBBCCDAA")))
  (is (= [[\A 1]] (run-length-encode "A")))
  (is (= [[\A 2]] (run-length-encode "AA")))
  (is (= [[\A 3] [\D 6] [\F 3] [\C 3] [\A 2] [\V 4]]
         (run-length-encode "AAADDDDDDFFFCCCAAVVVV")))
  (is (= [] (run-length-encode ""))))

(deftest test-decode
  (is (= "AAAABBBCCDAA"
         (run-length-decode [[\A 4] [\B 3] [\C 2] [\D 1] [\A 2]])))
  (is (= "A" (run-length-decode [[\A 1]])))
  (is (= "AA" (run-length-decode [[\A 2]])))
  (is (= "AAADDDDDDFFFCCCAAVVVV"
         (run-length-decode [[\A 3] [\D 6] [\F 3] [\C 3] [\A 2] [\V 4]])))
  (is (= "" (run-length-decode []))))

(deftest test-roundtrip
  (doseq [t ["AAAABBBCCDAA" "A" "" "abbbccca"]]
    (is (= t (run-length-decode (run-length-encode t))))))
