(ns clojure-algorithms.genetic-algorithm.travelling-salesman-problem-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.genetic-algorithm.travelling-salesman-problem
             :refer [cities distance init fitness chose-ts chose-rws crossing
                     mutate run-ga tour-length]]))

(deftest distance-test
  (is (= 5.0 (distance [0 0] [3 4])))
  (is (= 5.0 (distance [0 0] [-3 4])))
  (is (= 5.0 (distance [0 0] [-3 -4]))))

(deftest init-test
  (let [rng (java.util.Random. 0)
        [chromosomes cities-list] (init {0 [0 0] 1 [2 2]} 2 rng)]
    (is (= [[0 1 0] [0 1 0]] chromosomes))
    (is (= [1] cities-list)))
  (is (= [[] [1]] (init {0 [0 0] 1 [2 2]} 0 (java.util.Random. 0))))
  (is (thrown? clojure.lang.ExceptionInfo
               (init {} 2 (java.util.Random. 0)))))

(deftest fitness-test
  (let [[matrix path dist] (fitness {0 [0 0] 1 [2 2]} [[0 1 0]] [] Double/POSITIVE_INFINITY)]
    (is (< (Math/abs (- (first matrix) 0.17677669529663687)) 1e-12))
    (is (= [0 1 0] path))
    (is (< (Math/abs (- dist 5.656854249492381)) 1e-12)))
  (is (= [[] [] Double/POSITIVE_INFINITY]
         (fitness {0 [0 0] 1 [2 2]} [] [] Double/POSITIVE_INFINITY))))

(deftest selection-test
  (let [rng (java.util.Random. 0)]
    (is (= [[0 1 0]] (chose-ts rng [1] [[0 1 0]] 1)))
    (is (= [] (chose-ts rng [1] [[0 1 0]] 0)))
    (is (= [[0 1 0]] (chose-rws rng [1] [[0 1 0]] 1)))))

(deftest crossing-test
  (let [rng (java.util.Random. 0)]
    (is (= [[0 1 0] [0 1 0]]
           (crossing rng [0 1 0] [0 1 0] 0 [1])))
    (let [[a b] (crossing rng [0 1 0] [0 1 0] 1 [1])]
      (is (= [0 1 0] a))
      (is (= [0 1 0] b)))))

(deftest mutate-test
  (is (= [0 1 0] (mutate (java.util.Random. 0) [0 1 0] 0)))
  (let [m (mutate (java.util.Random. 1) [0 1 2 3 0] 1)]
    (is (= 0 (first m) (last m)))
    (is (= (sort [1 2 3]) (sort (subvec (vec m) 1 4)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (mutate (java.util.Random. 0) [] 1))))

(deftest run-ga-test
  (let [[path dist] (run-ga {0 [0 0] 1 [2 2]} 10 10 0.6 0.2
                            :rng (java.util.Random. 0))]
    (is (= [0 1 0] path))
    (is (< (Math/abs (- dist 5.656854249492381)) 1e-12)))
  (let [[path dist] (run-ga cities 100 30 0.6 0.2 :rng (java.util.Random. 0))]
    (is (= 0 (first path) (last path)))
    (is (= (sort (keys cities)) (sort (pop (vec path)))))
    (is (< (Math/abs (- dist (tour-length cities path))) 1e-9))))
