(ns clojure-algorithms.strings.rabin-karp-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.rabin-karp :refer [rabin-karp]]))

(deftest test-rabin-karp
  (testing "reference cases"
    (is (true? (rabin-karp "abc1abc12"
                           "alskfjaldsabc1abc1abc12k23adsfabcabc")))
    (is (false? (rabin-karp "abc1abc12" "alskfjaldsk23adsfabcabc")))
    (is (true? (rabin-karp "ABABX" "ABABZABABYABABX")))
    (is (true? (rabin-karp "AAAB" "ABAAAAAB")))
    (is (true? (rabin-karp "abcdabcy" "abcxabcdabxabcdabcdabcy")))
    (is (true? (rabin-karp "Lü" "Lüsai")))
    (is (false? (rabin-karp "Lue" "Lüsai"))))
  (testing "edges"
    (is (false? (rabin-karp "longpattern" "short")))
    (is (true? (rabin-karp "" "anything")))))
