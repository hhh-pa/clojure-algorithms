(ns clojure-algorithms.audio-filters.equal-loudness-filter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.audio-filters.equal-loudness-filter
             :refer [yulewalk make-equal-loudness-filter]]))

(deftest stub-test
  (testing "numpy/scipy-dependent API throws with an explanatory message"
    (is (thrown? UnsupportedOperationException (yulewalk 4 [0.0 0.5 1.0] [1.0 0.5 0.0])))
    (is (thrown? UnsupportedOperationException (make-equal-loudness-filter)))
    (is (thrown? UnsupportedOperationException (make-equal-loudness-filter 44100)))))
