(ns clojure-algorithms.digital-image-processing.filters.sobel-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.filters.sobel-filter
             :refer [sobel-filter]]))

(deftest test-sobel-filter
  (let [img (mapv (fn [i]
                    (mapv (fn [j] (if (and (>= i 3) (>= j 3)) 255 0))
                          (range 6)))
                  (range 6))
        [grad theta] (sobel-filter img)]
    (is (= 6 (count grad) (count theta)))
    (is (every? #(<= 0 % 255) (mapcat identity grad)))
    (is (some pos? (mapcat identity grad)))
    (is (some #(not (zero? %)) (mapcat identity theta))))
  (let [[grad _] (sobel-filter (vec (repeat 4 (vec (repeat 4 7)))))]
    (is (every? zero? (mapcat identity grad)))))
