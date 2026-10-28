(ns clojure-algorithms.maths.collatz-sequence-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.collatz-sequence :as sut]))

(deftest test-fn-collatz-sequence-examples
  (is (thrown? Exception (vec (sut/collatz-sequence 2.1))))
  (is (thrown? Exception (vec (sut/collatz-sequence 0))))
  (is (approx [4 2 1] (vec (sut/collatz-sequence 4))))
  (is (approx [11 34 17 52 26 13 40 20 10 5 16 8 4 2 1]
              (vec (sut/collatz-sequence 11))))
  (is (approx [31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137
               412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593
               1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425
               1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644
               1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732
               866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53
               160 80 40 20 10 5 16 8 4 2 1]
              (vec (sut/collatz-sequence 31))))
  (is (approx [43 130 65 196 98 49 148 74 37 112 56 28 14 7 22 11 34 17 52 26
               13 40 20 10 5 16 8 4 2 1]
              (vec (sut/collatz-sequence 43)))))

(deftest test-fn-collatz-sequence-extra
  (is (= :TypeError (try (sut/collatz-sequence 2.1) nil
                         (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :ValueError (try (sut/collatz-sequence 0) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (approx [1] (vec (sut/collatz-sequence 1))))
  (is (some? #'sut/main)))
