(ns clojure-algorithms.conversions.convert-number-to-words-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.convert-number-to-words
             :refer [convert-small-number convert-number max-value]]))

(deftest test-convert-small-number
  (is (= "zero" (convert-small-number 0)))
  (is (= "five" (convert-small-number 5)))
  (is (= "ten" (convert-small-number 10)))
  (is (= "fifteen" (convert-small-number 15)))
  (is (= "twenty" (convert-small-number 20)))
  (is (= "twenty-five" (convert-small-number 25)))
  (is (thrown? clojure.lang.ExceptionInfo (convert-small-number -1)))
  (is (thrown? clojure.lang.ExceptionInfo (convert-small-number 123))))

(deftest test-convert-number
  (is (= "zero" (convert-number 0)))
  (is (= "one" (convert-number 1)))
  (is (= "one hundred" (convert-number 100)))
  (is (= "negative one hundred" (convert-number -100)))
  (is (= "one hundred twenty-three million four hundred fifty-six thousand seven hundred eighty-nine"
         (convert-number 123456789)))
  (is (= (str "one hundred twenty-three trillion four hundred fifty-six billion "
              "seven hundred eighty-nine million twelve thousand "
              "three hundred forty-five")
         (convert-number 123456789012345)))
  (is (= "one thousand" (convert-number 1000 "long")))
  (is (= "one lakh" (convert-number 100000 "indian")))
  (is (thrown? clojure.lang.ExceptionInfo (convert-number (long (Math/pow 10 18)))))
  (is (= (dec (reduce * 1 (repeat 18 10N))) (max-value "short")))
  (is (= (dec (reduce * 1 (repeat 21 10N))) (max-value "long")))
  (is (= (dec (reduce * 1 (repeat 19 10N))) (max-value "indian"))))
