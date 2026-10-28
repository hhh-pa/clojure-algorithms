(ns clojure-algorithms.data-compression.lz77-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-compression.lz77
             :refer [make-compressor compress decompress find-encoding-token
                     match-length-from-index token->str ->Token]]))

(deftest test-compress
  (let [c (make-compressor)]
    (is (= "[(0, 0, a), (0, 0, b), (2, 2, c), (4, 3, a), (2, 2, a)]"
           (str "[" (clojure.string/join ", "
                                         (map token->str (compress c "ababcbababaa")))
                "]")))
    (is (= "[(0, 0, a), (1, 1, c), (3, 4, b), (3, 3, a), (1, 2, c)]"
           (str "[" (clojure.string/join ", "
                                         (map token->str (compress c "aacaacabcabaaac")))
                "]")))))

(deftest test-decompress
  (let [c (make-compressor)]
    (is (= "cabracadabrarrarrad"
           (decompress c [(->Token 0 0 \c) (->Token 0 0 \a)
                          (->Token 0 0 \b) (->Token 0 0 \r)
                          (->Token 3 1 \c) (->Token 2 1 \d)
                          (->Token 7 4 \r) (->Token 3 5 \d)])))
    (is (= "ababcbababaa"
           (decompress c [(->Token 0 0 \a) (->Token 0 0 \b)
                          (->Token 2 2 \c) (->Token 4 3 \a)
                          (->Token 2 2 \a)])))
    (is (= "aacaacabcabaaac"
           (decompress c [(->Token 0 0 \a) (->Token 1 1 \c)
                          (->Token 3 4 \b) (->Token 3 3 \a)
                          (->Token 1 2 \c)])))))

(deftest test-roundtrip
  (let [c (make-compressor)]
    (doseq [t ["cabracadabrarrarrad" "ababcbababaa" "aacaacabcabaaac"]]
      (is (= t (decompress c (compress c t))))))
  ;; Parity with Python: all-same-char input overruns the text index
  ;; (Python raises IndexError there).
  (let [c (make-compressor)]
    (is (thrown? StringIndexOutOfBoundsException (compress c "aaaaaa")))))

(deftest test-find-encoding-token
  (let [c (make-compressor)]
    (is (= 7 (:offset (find-encoding-token c "abrarrarrad" "abracad"))))
    (is (= 1 (:length (find-encoding-token c "adabrarrarrad" "cabrac"))))
    (is (= 0 (:offset (find-encoding-token c "abc" "xyz"))))
    (is (= 0 (:offset (find-encoding-token c "abc" ""))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (find-encoding-token c "" "xyz")))))

(deftest test-match-length
  (let [c (make-compressor 13 6)]
    (is (= 5 (match-length-from-index c "rarrad" "adabrar" 0 4)))
    (is (= 1 (match-length-from-index c "adabrarrarrad" "cabrac" 0 1)))))

(deftest test-token-str
  (is (= "(1, 2, c)" (token->str (->Token 1 2 \c)))))
