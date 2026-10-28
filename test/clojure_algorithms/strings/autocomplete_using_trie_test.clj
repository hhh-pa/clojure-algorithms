(ns clojure-algorithms.strings.autocomplete-using-trie-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.autocomplete-using-trie
             :refer [autocomplete-using-trie insert-word find-word
                     default-words]]))

(deftest test-autocomplete-using-trie
  (testing "doctest example"
    (let [matches (autocomplete-using-trie "de")]
      (is (some #{"detergent "} matches))
      (is (not (some #{"dog "} matches)))))
  (testing "custom trie"
    (let [trie (reduce insert-word {} default-words)]
      (is (= ["dog "] (autocomplete-using-trie trie "dog")))
      (is (= [] (find-word trie "xyz"))))))
