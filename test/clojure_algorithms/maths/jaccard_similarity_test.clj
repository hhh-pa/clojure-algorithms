(ns clojure-algorithms.maths.jaccard-similarity-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.jaccard-similarity :as sut]))

(deftest test-fn-jaccard-similarity-examples
  (is (approx 0.375 (sut/jaccard-similarity #{"a" "b" "c" "d" "e"}
                                            #{"c" "d" "e" "f" "h" "i"})))
  (is (approx 1.0 (sut/jaccard-similarity #{"a" "b" "c" "d" "e"}
                                          #{"a" "b" "c" "d" "e"})))
  (is (approx 0.5 (sut/jaccard-similarity #{"a" "b" "c" "d" "e"}
                                          #{"a" "b" "c" "d" "e"} true)))
  (is (approx 0.375 (sut/jaccard-similarity ["a" "b" "c" "d" "e"]
                                            ["c" "d" "e" "f" "h" "i"])))
  (is (approx 0.375 (sut/jaccard-similarity ["c" "d" "e" "f" "h" "i"]
                                            ["a" "b" "c" "d" "e"])))
  (is (approx 0.2 (sut/jaccard-similarity ["c" "d" "e" "f" "h" "i"]
                                          ["a" "b" "c" "d"] true)))
  (is (thrown? Exception (sut/jaccard-similarity #{"a" "b"} ["c" "d"]))))

(deftest test-fn-jaccard-similarity-extra
  (is (= :ValueError (try (sut/jaccard-similarity #{"a" "b"} ["c" "d"]) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (thrown? Exception (sut/jaccard-similarity ["c" "d"] #{"a" "b"})))
  (is (approx 0.0 (sut/jaccard-similarity #{"a"} #{"b"}))))
