(ns clojure-algorithms.machine-learning.astar-test
  (:require [clojure-algorithms.machine-learning.astar :as sut]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest test-fn-astar-examples
  (let [world (sut/make-Gridworld)
        start (assoc (sut/make-Cell) :position [0 0])
        goal (assoc (sut/make-Cell) :position [4 4])]
    (is (= [[0 0] [1 1] [2 2] [3 3] [4 4]] (sut/astar world start goal))))
  (testing "start equals goal"
    (let [world (sut/make-Gridworld)
          cell (assoc (sut/make-Cell) :position [2 2])]
      (is (= [[2 2]] (sut/astar world cell cell)))))
  (testing "adjacent cells"
    (let [world (sut/make-Gridworld)
          start (assoc (sut/make-Cell) :position [0 0])
          goal (assoc (sut/make-Cell) :position [0 1])]
      (is (= [[0 0] [0 1]] (sut/astar world start goal))))))

(deftest test-fn-cells-and-world
  (testing "cell equality is positional"
    (is (true? (sut/Cell-eq (sut/make-Cell) (sut/make-Cell))))
    (is (false? (sut/Cell-eq (sut/make-Cell)
                             (assoc (sut/make-Cell) :position [1 1])))))
  (testing "world defaults"
    (let [world (sut/make-Gridworld)]
      (is (= 5 (:world-x-limit world)))
      (is (= 5 (:world-y-limit world)))
      (is (= (vec (repeat 5 (vec (repeat 5 0)))) (:w world)))))
  (testing "custom world size"
    (let [world (sut/make-Gridworld [3 2])]
      (is (= 3 (:world-x-limit world)))
      (is (= 2 (:world-y-limit world)))))
  (testing "neighbour counts by position"
    (let [world (sut/make-Gridworld)]
      (is (= 3 (count (sut/Gridworld-get-neighbours
                       world (assoc (sut/make-Cell) :position [0 0])))))
      (is (= 5 (count (sut/Gridworld-get-neighbours
                       world (assoc (sut/make-Cell) :position [0 2])))))
      (is (= 8 (count (sut/Gridworld-get-neighbours
                       world (assoc (sut/make-Cell) :position [2 2])))))))
  (testing "neighbours are parented at the cell"
    (let [world (sut/make-Gridworld)
          cell (assoc (sut/make-Cell) :position [2 2])]
      (is (every? #(= cell (:parent %))
                  (sut/Gridworld-get-neighbours world cell)))))
  (testing "show helpers print"
    (is (str/includes? (with-out-str (sut/Cell-showcell (sut/make-Cell)))
                       "[0 0]"))
    (is (str/includes? (with-out-str
                         (sut/Gridworld-show (sut/make-Gridworld [1 1])))
                       "[[0]]"))))
