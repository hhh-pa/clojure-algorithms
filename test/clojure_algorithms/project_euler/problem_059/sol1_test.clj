(ns clojure-algorithms.project-euler.problem-059.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-059.sol1 :refer [try-key filter-valid-chars filter-common-word solution]]))

(deftest try-key-test
  (is (= "hello" (try-key [0 17 20 4 27] [104 116 120])))
  (is (nil? (try-key [68 10 300 4 27] [104 116 120]))))

(deftest filter-valid-chars-test
  (let [text "The enemy's gate is down"
        key (map int "end")
        encoded (mapv bit-xor (cycle key) (map int text))]
    (is (contains? (set (filter-valid-chars encoded)) text))))

(deftest filter-common-word-test
  (is (= ["I am here"]
         (filter-common-word ["asfla adf" "I am here" "   !?! #a"] "am")))
  (is (= ["athla amf" "I am here"]
         (filter-common-word ["athla amf" "I am here" "   !?! #a"] "am"))))

(deftest solution-test
  (is (= 10644 (solution (str "31,10,89,9,0,89,4,23,89,5,10,13,75,17,22,75,7,28,71,69,"
                              "13,3,4,13,75,12,10,75,17,17,14,69,8,30,0,10,31,12,22,5,94,89,"
                              "28,0,89,3,4,15,14,69,13,4,69,27,14,69,16,5,69,13,3,0,89,3,10,"
                              "12,24,0,89,4,3,89,8,4,11,15,22,89,10,11,29,75,21,21,10,28,89,"
                              "31,12,21,7,69,29,10,18,23,75,7,11,14,4,18,24,69,22,29,0,11,"
                              "75,17,17,14,69,17,2,9,21,24")))))
