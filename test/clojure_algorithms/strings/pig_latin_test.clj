(ns clojure-algorithms.strings.pig-latin-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.pig-latin :refer [pig-latin]]))

(deftest test-pig-latin
  (testing "doctest examples"
    (is (= "igpay" (pig-latin "pig")))
    (is (= "atinlay" (pig-latin "latin")))
    (is (= "ananabay" (pig-latin "banana")))
    (is (= "iendsfray" (pig-latin "friends")))
    (is (= "ilesmay" (pig-latin "smile")))
    (is (= "ingstray" (pig-latin "string")))
    (is (= "eatway" (pig-latin "eat")))
    (is (= "omeletway" (pig-latin "omelet")))
    (is (= "areway" (pig-latin "are")))
    (is (= "" (pig-latin " ")))
    (is (= "" (pig-latin nil)))))
