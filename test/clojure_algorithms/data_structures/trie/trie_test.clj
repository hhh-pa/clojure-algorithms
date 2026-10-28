(ns clojure-algorithms.data-structures.trie.trie-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.trie.trie
             :refer [make-trie-node trie-insert-many trie-insert trie-find
                     trie-delete trie-words]]))

(deftest test-trie
  (let [words ["banana" "bananas" "bandana" "band" "apple" "all" "beast"]
        root (trie-insert-many (make-trie-node) words)]
    (is (every? #(trie-find root %) words))
    (is (true? (trie-find root "banana")))
    (is (false? (trie-find root "bandanas")))
    (is (false? (trie-find root "apps")))
    (is (true? (trie-find root "apple")))
    (is (true? (trie-find root "all")))
    (is (= (set words) (set (trie-words root))))
    (let [root (trie-delete root "all")]
      (is (false? (trie-find root "all")))
      (let [root (trie-delete root "banana")]
        (is (false? (trie-find root "banana")))
        (is (true? (trie-find root "bananas"))))))
  (let [root (trie-insert (make-trie-node) "a")]
    (is (true? (trie-find root "a")))
    (is (false? (trie-find (trie-delete root "a") "a")))))
