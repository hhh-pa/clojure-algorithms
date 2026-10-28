(ns clojure-algorithms.strings.string-switch-case-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.string-switch-case
             :refer [split-input to-simple-case to-complex-case to-pascal-case
                     to-camel-case to-snake-case to-kebab-case]]))

(deftest test-helpers
  (is (= [["one" "two" "31235three4four"]]
         (split-input "one two 31235three4four")))
  (is (= "OneTwo31235three4four"
         (to-simple-case "one two 31235three4four")))
  (is (= "ThisShouldBeCombined" (to-simple-case "This should be combined")))
  (is (= "TheFirstLettersAreCapitalizedThenStringIsMerged"
         (to-simple-case "The first letters are capitalized, then string is merged")))
  (is (= "SpecialCharactersAreIgnored"
         (to-simple-case "special characters :, ', %, ^, $, are ignored")))
  (is (= "ONE_TWO_31235THREE4FOUR"
         (to-complex-case "one two 31235three4four" true "_")))
  (is (= "one-two-31235three4four"
         (to-complex-case "one two 31235three4four" false "-"))))

(deftest test-cases
  (is (= "OneTwo31235three4four" (to-pascal-case "one two 31235three4four")))
  (is (= "oneTwo31235three4four" (to-camel-case "one two 31235three4four")))
  (is (= "ONE_TWO_31235THREE4FOUR"
         (to-snake-case "one two 31235three4four" true)))
  (is (= "one_two_31235three4four"
         (to-snake-case "one two 31235three4four" false)))
  (is (= "ONE-TWO-31235THREE4FOUR"
         (to-kebab-case "one two 31235three4four" true)))
  (is (= "one-two-31235three4four"
         (to-kebab-case "one two 31235three4four" false))))
