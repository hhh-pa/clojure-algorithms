(ns clojure-algorithms.maths.weddles-rule-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.weddles-rule :as sut]))

(deftest test-fn-get-inputs-examples
  (is (= ["1/(1+x**2)" 1.0 -1.0]
         (let [sw (java.io.StringWriter.)]
           (binding [*out* sw]
             (with-in-str "1/(1+x**2)\n1.0\n-1.0\n" (sut/get-inputs)))))))

(deftest test-fn-safe-function-eval-examples
  (let [f (sut/safe-function-eval "x**2")]
    (is (approx 9 (f 3))))
  (let [f (sut/safe-function-eval "x + x**2")]
    (is (approx 6 (f 2)))))

(deftest test-fn-safe-function-eval-extra
  (is (approx 0.5 ((sut/safe-function-eval "1/(1+x**2)") 1)))
  (is (approx 1.0 ((sut/safe-function-eval "1/(1+x**2)") 0)))
  (is (approx 14 ((sut/safe-function-eval "2+3*4") 0)))
  (is (approx 20 ((sut/safe-function-eval "(2+3)*4") 0)))
  (is (approx 512 ((sut/safe-function-eval "2**3**2") 0)))
  (is (approx -4 ((sut/safe-function-eval "-2**2") 0)))
  (is (approx 3.0 ((sut/safe-function-eval "7//2") 0)))
  (is (approx 1.0 ((sut/safe-function-eval "7%3") 0)))
  (is (approx 0.0 ((sut/safe-function-eval "sin(0)") 0)))
  (is (approx 1.0 ((sut/safe-function-eval "cos(0)") 0)))
  (is (approx 3.0 ((sut/safe-function-eval "sqrt(9)") 0)))
  (is (thrown? Exception (sut/safe-function-eval "x +")))
  (is (thrown? Exception (sut/safe-function-eval "y")))
  (is (thrown? Exception (sut/safe-function-eval "")))
  (is (thrown? Exception (sut/safe-function-eval "(2+3"))))

(deftest test-fn-compute-table-examples
  (let [[table h] (sut/compute-table (sut/safe-function-eval "1/(1+x**2)") 1 -1 1)]
    (is (approx [0.5 0.69230769 0.9 1.0 0.9 0.69230769 0.5] table))
    (is (approx -0.3333333333333333 h))))

(deftest test-fn-apply-weights-examples
  (is (approx [4.33 1.0 5.196 0.0 -4.33]
              (sut/apply-weights [0.0 0.866 1.0 0.866 0.0 -0.866 -1.0]))))

(deftest test-fn-compute-solution-examples
  (is (approx 0.7853975
              (sut/compute-solution [4.33 6.0 0.0 -4.33]
                                    [0.0 0.866 1.0 0.866 0.0 -0.866 -1.0]
                                    0.5235983333333333))))
