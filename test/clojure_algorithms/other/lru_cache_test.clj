(ns clojure-algorithms.other.lru-cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.lru-cache :as c]))

(deftest lru-test
  (testing "doctest trace"
    (let [cache (-> (c/make-lru 2)
                    (c/cache-put 1 1)
                    (c/cache-put 2 2))
          [cache v1] (c/cache-get cache 1)]
      (is (= 1 v1))
      (let [cache (c/cache-put cache 3 3)
            [cache v2] (c/cache-get cache 2)]
        (is (nil? v2))
        (let [cache (c/cache-put cache 4 4)
              [cache v1b] (c/cache-get cache 1)]
          (is (nil? v1b))
          (let [[cache v3] (c/cache-get cache 3)
                [cache v4] (c/cache-get cache 4)]
            (is (= 3 v3))
            (is (= 4 v4))
            (is (= "CacheInfo(hits=3, misses=2, capacity=2, current size=2)"
                   (c/cache-info cache))))))))
  (testing "put overwrites and refreshes recency"
    (let [cache (-> (c/make-lru 2)
                    (c/cache-put 1 1)
                    (c/cache-put 2 2)
                    (c/cache-put 1 10)
                    (c/cache-put 3 3))
          [cache v2] (c/cache-get cache 2)
          [_ v1] (c/cache-get cache 1)]
      (is (nil? v2))
      (is (= 10 v1)))))
