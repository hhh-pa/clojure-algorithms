(ns clojure-algorithms.financial.kelly-criterion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.financial.kelly-criterion
             :refer [kelly-criterion kelly-criterion-extended fractional-kelly]]))

(defn- round4 [x] (/ (Math/round (* 10000.0 (double x))) 10000.0))

(deftest kelly-test
  (is (= 0.4 (round4 (kelly-criterion 0.6 2.0))))
  (is (= 0.1 (round4 (kelly-criterion 0.55 1.0))))
  (is (= 0.0 (kelly-criterion 0.5 1.0)))
  (is (= -0.05 (round4 (kelly-criterion 0.3 2.0))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"win_probability must be between"
                        (kelly-criterion 0.0 1.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"win_probability must be between"
                        (kelly-criterion 1.0 1.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"win_loss_ratio must be > 0"
                        (kelly-criterion 0.5 0.0))))

(deftest kelly-extended-test
  (is (= 0.4 (round4 (kelly-criterion-extended 0.6 2.0 1.0))))
  (is (= 0.0 (kelly-criterion-extended 0.5 1.0 1.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"win_amount must be > 0"
                        (kelly-criterion-extended 0.5 0.0 1.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"loss_amount must be > 0"
                        (kelly-criterion-extended 0.5 1.0 0.0))))

(deftest fractional-kelly-test
  (is (= 0.2 (round4 (fractional-kelly 0.6 2.0 0.5))))
  (is (= 0.025 (round4 (fractional-kelly 0.55 1.0 0.25))))
  (is (= 0.6 (round4 (fractional-kelly 0.7 3.0 1.0))))
  (is (= 0.2 (round4 (fractional-kelly 0.6 2.0))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"fraction must be between"
                        (fractional-kelly 0.6 2.0 0.0)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"fraction must be between"
                        (fractional-kelly 0.6 2.0 1.5))))
