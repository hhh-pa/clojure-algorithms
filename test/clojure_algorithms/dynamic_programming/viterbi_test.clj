(ns clojure-algorithms.dynamic-programming.viterbi-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.viterbi :as v]))

(def observations ["normal" "cold" "dizzy"])
(def states ["Healthy" "Fever"])
(def start-p {"Healthy" 0.6 "Fever" 0.4})
(def trans-p {"Healthy" {"Healthy" 0.7 "Fever" 0.3}
              "Fever" {"Healthy" 0.4 "Fever" 0.6}})
(def emit-p {"Healthy" {"normal" 0.5 "cold" 0.4 "dizzy" 0.1}
             "Fever" {"normal" 0.1 "cold" 0.3 "dizzy" 0.6}})

(deftest viterbi-test
  (testing "Wikipedia example"
    (is (= ["Healthy" "Healthy" "Fever"]
           (v/viterbi observations states start-p trans-p emit-p)))
    (is (= ["Healthy"]
           (v/viterbi ["normal"] states start-p trans-p emit-p))))
  (testing "empty parameters"
    (is (thrown? IllegalArgumentException
                 (v/viterbi [] states start-p trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations [] start-p trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states {} trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p {} emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p trans-p {})))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p trans-p nil))))
  (testing "malformed parameters"
    (is (thrown? IllegalArgumentException
                 (v/viterbi "invalid" states start-p trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi ["valid" 123] states start-p trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations "invalid" start-p trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations ["valid" 123] start-p trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states "invalid" trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states {2 2} trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states {"a" 2} trans-p emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p "invalid" emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p {"a" 2} emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p {2 {2 2}} emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p {"a" {2 2}} emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p {"a" {"b" 2}} emit-p)))
    (is (thrown? IllegalArgumentException
                 (v/viterbi observations states start-p trans-p "invalid")))))
