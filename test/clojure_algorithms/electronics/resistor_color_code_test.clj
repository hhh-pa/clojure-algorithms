(ns clojure-algorithms.electronics.resistor-color-code-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.resistor-color-code :as rc]))

(deftest getters-test
  (testing "band value getters"
    (is (= "06" (rc/get-significant-digits ["Black" "Blue"])))
    (is (thrown? IllegalArgumentException
                 (rc/get-significant-digits ["Aqua" "Blue"])))
    (is (= 0.1 (rc/get-multiplier "Gold")))
    (is (thrown? IllegalArgumentException (rc/get-multiplier "Ivory")))
    (is (= 0.5 (rc/get-tolerance "Green")))
    (is (thrown? IllegalArgumentException (rc/get-tolerance "Indigo")))
    (is (= 25 (rc/get-temperature-coefficient "Yellow")))
    (is (thrown? IllegalArgumentException
                 (rc/get-temperature-coefficient "Cyan")))
    (is (= 2 (rc/get-band-type-count 3 "significant")))
    (is (thrown? IllegalArgumentException (rc/get-band-type-count 2 "significant")))
    (is (thrown? IllegalArgumentException (rc/get-band-type-count 3 "sign")))
    (is (thrown? IllegalArgumentException (rc/get-band-type-count 3 "tolerance")))
    (is (thrown? IllegalArgumentException
                 (rc/get-band-type-count 5 "temp_coeffecient")))))

(deftest validity-test
  (testing "code validity"
    (is (true? (rc/check-validity 3 ["Black" "Blue" "Orange"])))
    (is (thrown? IllegalArgumentException
                 (rc/check-validity 4 ["Black" "Blue" "Orange"])))
    (is (thrown? IllegalArgumentException
                 (rc/check-validity 3 ["Cyan" "Red" "Yellow"])))))

(deftest resistance-test
  (testing "calculated resistances"
    (is (= {:resistance "6000\u03A9 \u00B120% "}
           (rc/calculate-resistance 3 ["Black" "Blue" "Orange"])))
    (is (= {:resistance "35000000\u03A9 \u00B15% "}
           (rc/calculate-resistance 4 ["Orange" "Green" "Blue" "Gold"])))
    (is (= {:resistance "7.18\u03A9 \u00B10.5% "}
           (rc/calculate-resistance 5 ["Violet" "Brown" "Grey" "Silver" "Green"])))
    (is (= {:resistance "2560000\u03A9 \u00B10.05% 1 ppm/K"}
           (rc/calculate-resistance 6 ["Red" "Green" "Blue" "Yellow" "Orange"
                                       "Grey"]))))
  (testing "invalid codes"
    (is (thrown? IllegalArgumentException
                 (rc/calculate-resistance 0 ["Violet" "Brown" "Grey" "Silver"
                                             "Green"])))
    (is (thrown? IllegalArgumentException
                 (rc/calculate-resistance 4 ["Violet" "Brown" "Grey" "Silver"
                                             "Green"])))
    (is (thrown? IllegalArgumentException
                 (rc/calculate-resistance 4 ["Violet" "Silver" "Brown" "Grey"])))
    (is (thrown? IllegalArgumentException
                 (rc/calculate-resistance 4 ["Violet" "Blue" "Lime" "Grey"])))))
