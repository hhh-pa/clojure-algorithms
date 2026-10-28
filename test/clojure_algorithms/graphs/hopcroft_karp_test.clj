(ns clojure-algorithms.graphs.hopcroft-karp-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.hopcroft-karp :refer [hopcroft-karp]]))

(deftest test-hopcroft-karp
  (is (= {"u1" "v2" "u2" "v1" "u3" "v3"}
         (hopcroft-karp {"u1" ["v1" "v2"] "u2" ["v1"] "u3" ["v2" "v3"]})))
  (is (= {} (hopcroft-karp {})))
  (is (= {} (hopcroft-karp {"u1" []})))
  (is (= {"u1" "v1"} (hopcroft-karp {"u1" ["v1"] "u2" ["v1"]})))
  (is (= {"u1" "v1" "u2" "v2" "u3" "v3"}
         (hopcroft-karp {"u1" ["v1" "v2"] "u2" ["v2" "v3"]
                         "u3" ["v3" "v1"]})))
  (is (thrown? IllegalArgumentException (hopcroft-karp {"A" ["A"]})))
  (is (thrown? IllegalArgumentException (hopcroft-karp {"u" [nil]}))))
