(ns clojure-algorithms.dynamic-programming.all-construct-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.all-construct :as ac]))

(deftest all-construct-test
  (testing "all constructions"
    (is (= [["he" "l" "l" "o"]]
           (ac/all-construct "hello" ["he" "l" "o"])))
    (is (= [["purp" "le"] ["p" "ur" "p" "le"]]
           (ac/all-construct "purple" ["purp" "p" "ur" "le" "purpl"])))
    (is (= [] (ac/all-construct "hello" ["x"])))
    (is (= [[]] (ac/all-construct "" ["a"])))
    (testing "every construction concatenates to the target"
      (doseq [ways [(ac/all-construct "jwajalapa"
                                      ["jwa" "j" "w" "a" "la" "lapa"])]]
        (doseq [w ways]
          (is (= "jwajalapa" (apply str w))))))))
