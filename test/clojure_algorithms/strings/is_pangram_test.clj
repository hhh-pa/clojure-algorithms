(ns clojure-algorithms.strings.is-pangram-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-pangram :refer [is-pangram
                                                           is-pangram-faster
                                                           is-pangram-fastest]]))

(def ^:private true-cases
  ["The quick brown fox jumps over the lazy dog"
   "Waltz, bad nymph, for quick jigs vex."
   "Jived fox nymph grabs quick waltz."])

(deftest test-pangram-variants
  (doseq [f [is-pangram is-pangram-faster is-pangram-fastest]]
    (testing (str f)
      (doseq [s true-cases]
        (is (true? (f s)) s))
      (is (false? (f "The quick brown fox jumps over the la_y dog")))
      (is (true? (f)))))
  (testing "non-pangram"
    (is (false? (is-pangram "My name is Unknown")))))
