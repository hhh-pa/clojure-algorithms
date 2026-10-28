(ns clojure-algorithms.other.word-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.word-search :as w]))

(deftest insert-test
  (testing "north"
    (is (= [[nil nil \t] [nil nil \a] [nil nil \c]]
           (w/insert-north (w/make-board 3 3) "cat" [2] [2]))))
  (testing "east"
    (is (= [[nil nil nil] [\c \a \t] [nil nil nil]]
           (w/insert-east (w/make-board 3 3) "cat" [1] [0]))))
  (testing "south"
    (is (= [[\c nil nil] [\a nil nil] [\t nil nil]]
           (w/insert-south (w/make-board 3 3) "cat" [0] [0]))))
  (testing "word that cannot fit leaves the board unchanged"
    (let [b (w/make-board 2 2)]
      (is (= b (w/insert-east b "toolong" [0 1] [0 1]))))))

(deftest generate-board-test
  (testing "all word letters end up on the board"
    (let [board (w/generate-board ["cat" "dog" "snake" "fish"] 10 10)
          letters (filter some? (apply concat board))]
      (is (= (+ 3 3 5 4) (count letters))))))

(deftest visualise-test
  (testing "empty cells render as # without fake chars"
    (let [board (w/insert-north (w/make-board 5 5) "cat" [4] [4])]
      (is (= (str "# # # # # \n"
                  "# # # # # \n"
                  "# # # # t \n"
                  "# # # # a \n"
                  "# # # # c \n")
             (w/visualise board false))))))
