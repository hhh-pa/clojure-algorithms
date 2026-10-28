(ns clojure-algorithms.digital-image-processing.dithering.burkes-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.dithering.burkes
             :refer [get-greyscale valid-threshold? burkes-process]]))

(deftest test-get-greyscale
  (is (< (Math/abs (- (get-greyscale 3 4 5) 4.185)) 1e-9))
  (is (= 0.0 (get-greyscale 0 0 0)))
  (is (< (Math/abs (- (get-greyscale 255 255 255) 255.0)) 1e-6)))

(deftest test-threshold
  (is (valid-threshold? 120))
  (is (not (valid-threshold? 0)))
  (is (not (valid-threshold? 255)))
  (is (thrown? clojure.lang.ExceptionInfo
               (burkes-process [[[0 0 0]]] 0))))

(deftest test-burkes-process
  (let [img [[[0 0 0] [255 255 255]]
             [[255 255 255] [0 0 0]]]
        out (burkes-process img 120)]
    (is (= [0 0 0] (get-in out [0 0])))
    (is (= [255 255 255] (get-in out [0 1])))
    (is (every? #{[0 0 0] [255 255 255]} (mapcat identity out))))
  (let [img (vec (repeat 4 (vec (repeat 4 [200 200 200]))))]
    (is (some #{[255 255 255]}
              (mapcat identity (burkes-process img 120))))))
