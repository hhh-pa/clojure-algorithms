(ns clojure-algorithms.sorts.external-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure-algorithms.sorts.external-sort :refer [external-sort parse-memory]])
  (:import [java.io File]))

(deftest test-parse-memory
  (is (= 100 (parse-memory "100")))
  (is (= (* 64 1024) (parse-memory "64K")))
  (is (= (* 100 1024 1024) (parse-memory "100M")))
  (is (= (* 1 1024 1024 1024) (parse-memory "1g"))))

(deftest test-external-sort
  (testing "sorts a file larger than one block"
    (let [in (File/createTempFile "extsort_in_" ".txt")
          lines (vec (repeatedly 500 #(str (rand-int 10000))))]
      (spit in (apply str (map #(str % "\n") lines)))
      (let [out (external-sort (.getPath in) 512)]
        (try
          (is (= (sort lines)
                 (with-open [r (io/reader out)] (doall (line-seq r)))))
          (finally
            (io/delete-file in true)
            (io/delete-file out true))))))
  (testing "custom sort key"
    (let [in (File/createTempFile "extsort_key_" ".txt")
          lines ["ccc" "a" "bb" "dddd" "ee"]]
      (spit in (apply str (map #(str % "\n") lines)))
      (let [out (external-sort (.getPath in) 8 count)]
        (try
          (is (= (sort-by count lines)
                 (with-open [r (io/reader out)] (doall (line-seq r)))))
          (finally
            (io/delete-file in true)
            (io/delete-file out true)))))))
