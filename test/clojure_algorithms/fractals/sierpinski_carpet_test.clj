(ns clojure-algorithms.fractals.sierpinski-carpet-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.sierpinski-carpet
             :refer [filled? generate-carpet count-filled-cells]]))

(deftest filled-test
  (is (true? (filled? 0 0)))
  (is (false? (filled? 1 1)))
  (is (false? (filled? 4 4)))
  (is (true? (filled? 0 4)))
  (is (= [true false true] (mapv #(filled? 1 %) (range 3))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"non-negative"
                        (filled? -1 0))))

(deftest carpet-test
  (is (= ["#"] (generate-carpet 0)))
  (is (= ["###" "# #" "###"] (generate-carpet 1)))
  (is (= ["XXXXXXXXX" "X.XX.XX.X" "XXXXXXXXX"
          "XXX...XXX" "X.X...X.X" "XXX...XXX"
          "XXXXXXXXX" "X.XX.XX.X" "XXXXXXXXX"]
         (generate-carpet 2 "X" ".")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"non-negative"
                        (generate-carpet -1))))

(deftest count-test
  (is (= [1 8 64 512] (mapv count-filled-cells (range 4))))
  (is (every? true?
              (map (fn [depth]
                     (= (count-filled-cells depth)
                        (reduce + (map #(count (filter #{\#} %)) (generate-carpet depth)))))
                   (range 4)))))
