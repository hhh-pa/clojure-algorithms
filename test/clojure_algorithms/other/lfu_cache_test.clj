(ns clojure-algorithms.other.lfu-cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.lfu-cache :as c]))

(deftest lfu-test
  ;; NOTE: the Python doctest's `put(4,4)` step evicts key 1 (freq 2)
  ;; instead of key 3 (freq 1) due to a corrupted-list bug in
  ;; `_position_node`; this port evicts the true LFU key.
  (testing "eviction trace with correct LFU semantics"
    (let [cache (-> (c/make-lfu 2)
                    (c/cache-put 1 1)
                    (c/cache-put 2 2))
          [cache v1] (c/cache-get cache 1)]
      (is (= 1 v1))
      (let [cache (c/cache-put cache 3 3)
            [cache v2] (c/cache-get cache 2)]
        (is (nil? v2))
        (let [cache (c/cache-put cache 4 4)
              [cache v1b] (c/cache-get cache 1)]
          (is (= 1 v1b))
          (let [[cache v3] (c/cache-get cache 3)
                [cache v4] (c/cache-get cache 4)]
            (is (nil? v3))
            (is (= 4 v4))
            (is (= "CacheInfo(hits=3, misses=2, capacity=2, current_size=2)"
                   (c/cache-info cache))))))))
  (testing "missing key counts a miss"
    (let [[cache v] (c/cache-get (c/make-lfu 1) 1)]
      (is (nil? v))
      (is (= 1 (:misses cache))))))
