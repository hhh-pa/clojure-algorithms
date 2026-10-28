(ns clojure-algorithms.computer-vision.harris-corner-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.harris-corner :as h]))
(deftest harris-test
  (let [det (h/make-harris 0.04 3)]
    (is (= [] (h/detect det (vec (repeat 5 (vec (repeat 5 0)))))))
    (is (vector? (h/detect det [[0 0 0 0 0] [0 255 255 0 0] [0 255 255 0 0]
                                [0 0 0 0 0] [0 0 0 0 0]]))))
  (is (thrown? IllegalArgumentException (h/make-harris 0.05 3))))
