(ns clojure-algorithms.strings.bpe-tokenizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.bpe-tokenizer :refer [get-byte-pair-counts
                                                              merge-pair encode
                                                              decode
                                                              make-tokenizer]]))

(deftest test-get-byte-pair-counts
  (is (= {[73 32] 1 [32 97] 1 [97 109] 1 [109 32] 1 [32 74] 1 [74 111] 1
          [111 110] 1 [110 83] 1 [83 110] 1 [110 111] 1 [111 119] 1 [119 46] 1}
         (get-byte-pair-counts [73 32 97 109 32 74 111 110 83 110 111 119 46])))
  (is (= {[2 3] 2 [3 6] 2 [6 2] 2 [2 5] 1}
         (get-byte-pair-counts [2 3 6 2 3 6 2 5]))))

(deftest test-merge-pair
  (is (= [256 6 256 6 2 5]
         (merge-pair [2 3 6 2 3 6 2 5] [2 3] 256))))

(deftest test-encode-decode
  (testing "no repeated pairs -> raw bytes"
    (is (= [73 32 97 109 32 74 111 110 83 110 111 119 46]
           (:ids (encode "I am JonSnow."))))
    (is (= [] (:ids (encode "")))))
  (testing "round trip with merges"
    (let [{:keys [ids merges]} (encode (make-tokenizer 20) "abababab")]
      (is (= "abababab" (decode ids merges)))))
  (testing "decode without merges"
    (is (= "I am JonSnow."
           (decode [73 32 97 109 32 74 111 110 83 110 111 119 46])))))
