(ns clojure-algorithms.strings.barcode-validator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.barcode-validator :refer [get-check-digit
                                                                  valid?
                                                                  get-barcode]]))

(deftest test-get-check-digit
  (is (= 9 (get-check-digit 8718452538119)))
  (is (= 5 (get-check-digit 87184523)))
  (is (= 9 (get-check-digit 87193425381086)))
  (is (= [0 7 4 1 8 5 2 9 6 3]
         (mapv get-check-digit (range 0 100 10)))))

(deftest test-valid?
  (is (true? (valid? 8718452538119)))
  (is (false? (valid? 87184525)))
  (is (false? (valid? 87193425381089)))
  (is (false? (valid? 0))))

(deftest test-get-barcode
  (is (= 8718452538119 (get-barcode "8718452538119")))
  (is (thrown? IllegalArgumentException (get-barcode "dwefgiweuf")))
  (is (thrown? IllegalArgumentException (get-barcode "-5"))))
