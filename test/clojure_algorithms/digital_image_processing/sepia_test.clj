(ns clojure-algorithms.digital-image-processing.sepia-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.sepia
             :refer [to-grayscale make-sepia]]))

(deftest test-sepia
  (is (< (Math/abs (- (to-grayscale 0 0 100) 21.26)) 1e-9))
  (let [img [[[100 150 200]]]
        grey (int (+ (* 0.2126 200) (* 0.587 150) (* 0.114 100)))]
    (is (= [[[grey (+ grey 20) (+ grey 40)]]] (make-sepia img 20))))
  (let [img [[[255 255 255]]]]
    ;; grey = int(255 * (0.2126 + 0.587 + 0.114)) = 232
    (is (= [[[232 252 255]]] (make-sepia img 20))))
  (is (= [[[0 10 20]]] (make-sepia [[[0 0 0]]] 10))))
