(ns clojure-algorithms.strings.ngram-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.ngram :refer [create-ngram]]))

(deftest test-create-ngram
  (testing "doctest examples"
    (is (= ["I " " a" "am" "m " " a" "a " " s" "se" "en" "nt" "te" "en" "nc" "ce"]
           (create-ngram "I am a sentence" 2)))
    (is (= ["I " " a" "am" "m " " a" "an" "n " " N" "NL" "LP" "Pe" "er"]
           (create-ngram "I am an NLPer" 2)))
    (is (= [] (create-ngram "This is short" 50)))))
