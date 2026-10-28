(ns clojure-algorithms.digital-image-processing.change-brightness-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.change-brightness
             :refer [change-brightness]]))

(deftest test-change-brightness
  (let [img [[[100 100 100] [100 100 100]]
             [[100 100 100] [100 100 100]]]]
    (is (= [150 150 150] (get-in (change-brightness img 50) [0 0])))
    (is (= [50 50 50] (get-in (change-brightness img -50) [0 0])))
    (is (= [255 255 255] (get-in (change-brightness img 255) [0 0])))
    (is (thrown? clojure.lang.ExceptionInfo (change-brightness img 300)))
    (is (thrown? clojure.lang.ExceptionInfo (change-brightness img -256))))
  (is (= [[150 200]] (change-brightness [[100 150]] 50)))
  (is (= [[0 5]] (change-brightness [[100 105]] -100))))
