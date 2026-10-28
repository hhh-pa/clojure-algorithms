(ns clojure-algorithms.digital-image-processing.filters.bilateral-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.bilateral-filter
             :refer [vec-gaussian get-slice get-gauss-kernel bilateral-filter
                     parse-args]]))

(deftest test-vec-gaussian
  (let [g (vec-gaussian 0.0 1.0)]
    (is (< (Math/abs (- g (/ 1 (Math/sqrt (* 2 Math/PI))))) 1e-12)))
  (is (= 2 (count (vec-gaussian [0.0 1.0] 1.0)))))

(deftest test-gauss-kernel
  (let [k (get-gauss-kernel 3 1.0)]
    (is (= 3 (count k) (count (first k))))
    (is (> (get-in k [1 1]) (get-in k [0 0])))))

(deftest test-bilateral-flat
  (let [img (vec (repeat 7 (vec (repeat 7 0.5))))
        out (bilateral-filter img 1.0 1.0 3)]
    (doseq [i (range 1 6) j (range 1 6)]
      (is (< (Math/abs (- (get-in out [i j]) 0.5)) 1e-9)))
    (is (zero? (get-in out [0 0])))))

(deftest test-parse-args
  (is (= ["../image_data/lena.jpg" 1.0 1.0 5] (parse-args ["prog"])))
  (is (= ["f.jpg" 2.0 3.0 5] (parse-args ["prog" "f.jpg" "2.0" "3.0" "4"])))
  (is (= ["f.jpg" 2.0 3.0 5] (parse-args ["prog" "f.jpg" "2.0" "3.0" "5"]))))
