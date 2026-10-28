(ns clojure-algorithms.sorts.gnome-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.gnome-sort :refer [gnome-sort]]))

(deftest test-gnome-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (gnome-sort [0 5 3 2 2])))
    (is (= [] (gnome-sort [])))
    (is (= [-45 -5 -2] (gnome-sort [-2 -5 -45])))
    (is (= " !Gadeimnoprstu"
           (apply str (gnome-sort (set "Gnomes are stupid!"))))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 80 #(rand-int 100)))]
      (is (= (vec (sort data)) (gnome-sort data))))))
