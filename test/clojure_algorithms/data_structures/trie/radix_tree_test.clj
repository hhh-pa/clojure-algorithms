(ns clojure-algorithms.data-structures.trie.radix-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.trie.radix-tree
             :refer [make-radix-node node-match radix-insert radix-insert-many
                     radix-find radix-delete tree-str]]))

(deftest test-match
  (is (= ["my" "prefix" "string"]
         (node-match "myprefix" "mystring"))))

(deftest test-radix-insert-find
  (let [root (radix-insert-many (make-radix-node)
                                ["myprefix" "myprefixA" "myprefixAA"])]
    (is (= "- myprefix   (leaf)\n-- A   (leaf)\n--- A   (leaf)"
           (tree-str root))))
  (is (false? (radix-find (make-radix-node "myprefix") "mystring")))
  (is (= [false (make-radix-node "myprefix")]
         [(first (radix-delete (make-radix-node "myprefix") "mystring"))
          (make-radix-node "myprefix")]))
  (let [words ["banana" "bananas" "bandana" "band" "apple" "all"
               "beast"]
        root (radix-insert-many (make-radix-node) words)]
    (is (every? #(radix-find root %) words))
    (is (false? (radix-find root "bandanas")))
    (is (false? (radix-find root "apps")))
    (let [[deleted? root] (radix-delete root "all")]
      (is (true? deleted?))
      (is (false? (radix-find root "all")))
      (let [[deleted? root] (radix-delete root "banana")]
        (is (true? deleted?))
        (is (false? (radix-find root "banana")))
        (is (true? (radix-find root "bananas")))))))

(deftest test-radix-roundtrip
  (let [words ["romane" "romanus" "romulus" "rubens" "ruber" "rubicon"
               "rubicundus"]
        root (radix-insert-many (make-radix-node) words)]
    (is (every? #(radix-find root %) words))
    (let [root (reduce (fn [r w] (second (radix-delete r w))) root words)]
      (is (every? #(not (radix-find root %)) words)))))
