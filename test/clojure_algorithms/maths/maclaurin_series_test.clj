(ns clojure-algorithms.maths.maclaurin-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.maclaurin-series :as sut]))

(deftest test-fn-maclaurin-sin-examples
  (is (every? true? (map (fn [x] (approx (sut/maclaurin-sin x 50) (Math/sin x)))
                         (range -25 25))))
  (is (approx -0.5440211108893691 (sut/maclaurin-sin 10)))
  (is (approx 0.5440211108893704 (sut/maclaurin-sin -10)))
  (is (approx -0.544021110889369 (sut/maclaurin-sin 10 15)))
  (is (approx 0.5440211108893704 (sut/maclaurin-sin -10 15)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-sin "10")))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-sin 10 -30)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-sin 10 30.5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-sin 10 "30"))))

(deftest test-fn-maclaurin-cos-examples
  (is (every? true? (map (fn [x] (approx (sut/maclaurin-cos x 50) (Math/cos x)))
                         (range -25 25))))
  (is (approx 0.2836621854632268 (sut/maclaurin-cos 5)))
  (is (approx 0.2836621854632265 (sut/maclaurin-cos -5)))
  (is (approx -0.8390715290764524 (sut/maclaurin-cos 10 15)))
  (is (approx -0.8390715290764521 (sut/maclaurin-cos -10 15)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-cos "10")))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-cos 10 -30)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-cos 10 30.5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/maclaurin-cos 10 "30"))))
