(ns clojure-algorithms.data-compression.huffman-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.huffman
             :refer [parse-text build-tree traverse-tree huffman-codes
                     encode-text]]))

(deftest test-parse-text
  (let [letters (parse-text "aabbbc")]
    (is (= [1 2 3] (mapv :freq letters)))
    (is (= [\c \a \b] (mapv :letter letters)))))

(deftest test-huffman-codes
  (is (= {\b "0" \c "10" \a "11"} (huffman-codes "aabbbc"))))

(deftest test-encode-text
  (is (= "111100010" (encode-text "aabbbc")))
  (let [codes (huffman-codes "hello world")]
    (is (= (count "hello world")
           (count (filter #(contains? codes %) "hello world"))))
    (is (= (apply str (map codes "hello world"))
           (encode-text "hello world")))))
