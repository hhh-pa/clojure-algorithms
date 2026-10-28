(ns clojure-algorithms.strings.frequency-finder-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.frequency-finder :refer [get-frequency-order
                                                                 english-freq-match-score
                                                                 get-letter-count]]))

(deftest test-get-frequency-order
  (is (= "LOWDRHEZQXJKVBPYGFMUCSNIAT"
         (get-frequency-order "Hello World")))
  (is (= "LHOEZQXJKVBPYGFWMUCDRSNIAT"
         (get-frequency-order "Hello@")))
  (is (= "HZQXJKVBPYGFWMUCLDRSNIOATE"
         (get-frequency-order "h"))))

(deftest test-english-freq-match-score
  (is (= 1 (english-freq-match-score "Hello World"))))

(deftest test-get-letter-count
  (let [counts (get-letter-count "Hello")]
    (is (= 1 (counts \H)))
    (is (= 2 (counts \L)))
    (is (= 0 (counts \Z)))))
