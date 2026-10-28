(ns clojure-algorithms.strings.text-justification-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.text-justification :refer [text-justification]]))

(deftest test-text-justification
  (testing "doctest examples"
    (is (= ["This    is    an" "example  of text" "justification.  "]
           (text-justification "This is an example of text justification." 16)))
    (is (= ["Two        roads" "diverged   in  a" "yellow wood     "]
           (text-justification "Two roads diverged in a yellow wood" 16))))
  (testing "line widths"
    (doseq [line (text-justification "Two roads diverged in a yellow wood" 16)]
      (is (= 16 (count line))))))
