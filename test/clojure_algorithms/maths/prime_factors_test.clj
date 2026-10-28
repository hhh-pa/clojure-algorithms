(ns clojure-algorithms.maths.prime-factors-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.prime-factors :as sut]))

(def ^:private big-10-241
  "10**241 as an exact integer."
  (reduce *' 1 (repeat 241 10)))

(deftest test-fn-prime-factors-examples
  (is (approx [] (sut/prime-factors 0)))
  (is (approx [2 2 5 5] (sut/prime-factors 100)))
  (is (approx [2 2 2 2 2 2 2 2 2 5] (sut/prime-factors 2560)))
  (is (approx [] (sut/prime-factors 0.01)))
  (is (approx [] (sut/prime-factors 0.02)))
  (is (approx [] (sut/prime-factors 0.0)))
  (is (= (vec (concat (repeat 241 2) (repeat 241 5)))
         (sut/prime-factors big-10-241)))
  (is (thrown? Exception (sut/prime-factors "hello")))
  (is (thrown? Exception (sut/prime-factors [1 2 "hello"]))))

(deftest test-fn-unique-prime-factors-examples
  (is (approx [] (sut/unique-prime-factors 0)))
  (is (approx [2 5] (sut/unique-prime-factors 100)))
  (is (approx [2 5] (sut/unique-prime-factors 2560)))
  (is (approx [] (sut/unique-prime-factors 0.01)))
  (is (approx [] (sut/unique-prime-factors 0.02)))
  (is (approx [2 5] (sut/unique-prime-factors big-10-241)))
  (is (approx [] (sut/unique-prime-factors 0.0)))
  (is (thrown? Exception (sut/unique-prime-factors "hello")))
  (is (thrown? Exception (sut/unique-prime-factors [1 2 "hello"]))))

(deftest test-fn-prime-factors-extra
  (is (approx [] (sut/prime-factors 1)))
  (is (approx [2] (sut/prime-factors 2)))
  (is (approx [7] (sut/prime-factors 7)))
  (is (approx [] (sut/prime-factors -5)))
  (is (= :TypeError (try (sut/prime-factors "hello")
                         nil
                         (catch clojure.lang.ExceptionInfo e
                           (:type (ex-data e)))))))
