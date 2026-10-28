(ns clojure-algorithms.genetic-algorithm.basic-string-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.genetic-algorithm.basic-string
             :refer [evaluate crossover mutate basic]]))

(deftest evaluate-test
  (is (= ["Helxo Worlx" 9.0] (evaluate "Helxo Worlx" "Hello World")))
  (is (= ["abc" 3.0] (evaluate "abc" "abc")))
  (is (= ["xyz" 0.0] (evaluate "xyz" "abc"))))

(deftest crossover-test
  (let [rng (java.util.Random. 42)
        [c1 c2] (crossover rng "123456" "abcdef")]
    (is (= 6 (count c1) (count c2)))
    ;; each child is a recombination: prefix of one parent + suffix of the other
    (is (some true? (map (fn [i] (and (= (subs c1 0 i) (subs "123456" 0 i))
                                      (= (subs c1 i) (subs "abcdef" i))))
                         (range 7))))
    (is (some true? (map (fn [i] (and (= (subs c2 0 i) (subs "abcdef" 0 i))
                                      (= (subs c2 i) (subs "123456" i))))
                         (range 7))))))

(deftest mutate-test
  (let [genes (vec "ABCDEF")]
    (is (= "123456" (mutate (java.util.Random. 1) "123456" genes 0.0)))
    (let [m (mutate (java.util.Random. 123) "123456" genes 1.0)]
      (is (= 6 (count m)))
      (is (= 1 (count (filter true? (map (fn [a b] (not= a b)) m "123456")))))
      (is (some (set genes) m)))))

(deftest basic-test
  (let [[gen total best] (basic "hi" (vec "hi") :rng (java.util.Random. 1) :debug false)]
    (is (= "hi" best))
    (is (>= gen 1))
    (is (>= total 1)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not in genes list"
                        (basic "test" (vec "abcd") :debug false)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be bigger than"
                        (basic "ab" (vec "ab") :n-population 10 :n-selected 50
                               :debug false))))
