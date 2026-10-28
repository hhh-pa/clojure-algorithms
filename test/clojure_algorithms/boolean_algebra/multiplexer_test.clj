(ns clojure-algorithms.boolean-algebra.multiplexer-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.multiplexer :as m]))
(deftest mux-test
  (is (= 0 (m/mux 0 1 0)))
  (is (= 1 (m/mux 0 1 1)))
  (is (= 1 (m/mux 1 0 0)))
  (is (= 0 (m/mux 1 0 1)))
  (is (thrown? IllegalArgumentException (m/mux 2 1 0)))
  (is (thrown? IllegalArgumentException (m/mux 0 -1 0)))
  (is (thrown? IllegalArgumentException (m/mux 0 1 1.1))))
