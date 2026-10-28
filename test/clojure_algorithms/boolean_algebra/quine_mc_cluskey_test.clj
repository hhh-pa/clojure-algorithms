(ns clojure-algorithms.boolean-algebra.quine-mc-cluskey-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.boolean-algebra.quine-mc-cluskey :as q]))
(deftest compare-test
  (is (= "0_10" (q/compare-string "0010" "0110")))
  (is (= false (q/compare-string "0110" "1101"))))
(deftest check-test
  (is (= ["00"] (q/check ["00"]))))
(deftest binary-test
  (is (= ["001" "010"] (q/decimal-to-binary 3 [1 2]))))
(deftest table-test
  (is (true? (q/is-for-table "__1" "011" 2)))
  (is (false? (q/is-for-table "01_" "001" 1)))
  (is (= [[1]] (q/prime-implicant-chart ["0"] ["0"]))))
(deftest selection-test
  (is (= ["a"] (q/selection [[1]] ["a"]))))
