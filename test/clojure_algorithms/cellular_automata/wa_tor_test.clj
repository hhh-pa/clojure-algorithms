(ns clojure-algorithms.cellular-automata.wa-tor-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure-algorithms.cellular-automata.wa-tor :as w]))
(deftest entity-test
  (let [e (w/make-entity true [0 0])]
    (is (true? (:prey e)))
    (is (= [0 0] (:coords e)))
    (is (= w/prey-reproduction-time (:repro e)))))
(deftest planet-test
  (let [p (w/make-planet 10 15)]
    (is (= 10 (:width p)))
    (is (= 15 (:height p)))
    (is (= (+ w/predator-initial-count w/prey-initial-count)
           (count (w/get-entities p))))))
(deftest move-test
  (let [e (w/make-entity true [1 1])
        st {:width 3 :height 3
            :grid [[nil nil nil] [nil e nil] [nil nil nil]]}
        st' (w/move-and-reproduce st [1 1] ["N"])]
    (is (nil? (get-in (:grid st') [1 1])))
    (is (some? (get-in (:grid st') [0 1])))))
(deftest prey-around-test
  (let [st {:width 3 :height 3
            :grid [[nil (w/make-entity true [0 1]) nil]
                   [nil (w/make-entity false [1 1]) nil]
                   [nil (w/make-entity true [2 1]) nil]]}]
    (is (= 2 (count (w/get-surrounding-prey st (w/make-entity false [1 1])))))))
(deftest render-test
  (let [st {:width 3 :height 1
            :grid [[(w/make-entity true [0 0])
                    (w/make-entity false [0 1]) nil]]}]
    (is (str/includes? (w/render-planet st 0) "Prey count: 1"))))
