(ns clojure-algorithms.other.magicdiamondpattern-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.magicdiamondpattern :as m]))

(deftest diamond-test
  (testing "floyd"
    (is (= "  * \n * * \n* * * \n" (m/floyd 3))))
  (testing "reverse-floyd"
    (is (= "* * * \n * * \n  * \n   " (m/reverse-floyd 3))))
  (testing "pretty-print"
    (is (= "  * \n * * \n* * * \n* * * \n * * \n  * \n   "
           (m/pretty-print 3)))
    (is (= "       ...       ....        nothing printing :("
           (m/pretty-print 0)))))
