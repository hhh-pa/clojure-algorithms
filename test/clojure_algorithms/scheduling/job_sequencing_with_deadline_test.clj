(ns clojure-algorithms.scheduling.job-sequencing-with-deadline-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.job-sequencing-with-deadline :as jsd]))

(deftest job-sequencing-with-deadlines-test
  (testing "[job-count max-profit] results"
    (is (= [2 60]
           (jsd/job-sequencing-with-deadlines
            [[1 4 20] [2 1 10] [3 1 40] [4 1 30]])))
    (is (= [2 127]
           (jsd/job-sequencing-with-deadlines
            [[1 2 100] [2 1 19] [3 2 27] [4 1 25] [5 1 15]])))))
