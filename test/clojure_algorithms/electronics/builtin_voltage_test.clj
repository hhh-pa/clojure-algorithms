(ns clojure-algorithms.electronics.builtin-voltage-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.builtin-voltage :as bv]))

(deftest builtin-voltage-test
  (testing "built-in voltage value"
    (let [v (bv/builtin-voltage 1e17 1e17 1e10)]
      (is (< (Math/abs (- v 0.833370010652644)) 1e-12))))
  (testing "invalid concentrations"
    (is (thrown? IllegalArgumentException (bv/builtin-voltage 0 1600 200)))
    (is (thrown? IllegalArgumentException (bv/builtin-voltage 1000 0 1200)))
    (is (thrown? IllegalArgumentException (bv/builtin-voltage 1000 1000 0)))
    (is (thrown? IllegalArgumentException (bv/builtin-voltage 1000 3000 2000)))
    (is (thrown? IllegalArgumentException (bv/builtin-voltage 3000 1000 2000)))))
