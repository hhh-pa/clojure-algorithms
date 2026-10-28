(ns clojure-algorithms.maths.gamma-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.gamma :as sut]))

(defn- rel-close?
  "True when `actual` is within relative tolerance `tol` of `expected`."
  [expected actual tol]
  (< (/ (Math/abs (- (double actual) (double expected)))
        (Math/abs (double expected)))
     tol))

(deftest test-fn-gamma-iterative-examples
  (is (thrown? Exception (sut/gamma-iterative -1)))
  (is (thrown? Exception (sut/gamma-iterative 0)))
  (is (rel-close? 40320.0 (sut/gamma-iterative 9) 1e-9))
  (is (rel-close? 2.6834373819557675 (sut/gamma-iterative 3.3) 1e-9))
  (is (rel-close? 1.7724538509055159 (sut/gamma-iterative 0.5) 1e-9))
  (is (every? (fn [i]
                (rel-close? (reduce *' 1 (range 1 i)) (sut/gamma-iterative i) 1e-9))
              (range 1 13)))
  (is (= :ValueError (try (sut/gamma-iterative -1) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))

(deftest test-fn-integrand-examples
  (is (approx 0.5413411329464508 (sut/integrand 2 3)))
  (is (approx 0.14936120510359183 (sut/integrand 3 2))))

(deftest test-fn-gamma-recursive-examples
  (is (approx 1.7724538509055159 (sut/gamma-recursive 0.5)))
  (is (approx 1.0 (sut/gamma-recursive 1)))
  (is (approx 1.0 (sut/gamma-recursive 2)))
  (is (approx 3.3233509704478426 (sut/gamma-recursive 3.5)))
  (is (rel-close? 9.483367566824795e+307 (sut/gamma-recursive 171.5) 1e-12))
  (is (thrown? Exception (sut/gamma-recursive 0)))
  (is (thrown? Exception (sut/gamma-recursive -1.1)))
  (is (thrown? Exception (sut/gamma-recursive -4)))
  (is (thrown? Exception (sut/gamma-recursive 172)))
  (is (thrown? Exception (sut/gamma-recursive 1.1)))
  (is (= :ValueError (try (sut/gamma-recursive 0) nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :OverflowError (try (sut/gamma-recursive 172) nil
                             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (= :NotImplementedError (try (sut/gamma-recursive 1.1) nil
                                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
