(ns clojure-algorithms.other.lru-cache
  "Least-recently-used key/value cache (functional).

  Port of TheAlgorithms/Python `other/lru_cache.py`.
  A cache is `{:capacity, :entries {k {:val, :tick}}, :tick, :hits, :misses}`;
  `cache-get` returns `[updated-cache value-or-nil]`.")

(defn make-lru
  "Create an empty LRU cache with `capacity` slots."
  [capacity]
  {:capacity capacity :entries {} :tick 0 :hits 0 :misses 0})

(defn cache-info
  "Summary string mirroring the Python CacheInfo repr."
  [cache]
  (str "CacheInfo(hits=" (:hits cache)
       ", misses=" (:misses cache)
       ", capacity=" (:capacity cache)
       ", current size=" (count (:entries cache)) ")"))

(defn cache-get
  "Look up `k`, marking it most-recently-used; return `[cache value-or-nil]`."
  [cache k]
  (if (contains? (:entries cache) k)
    (let [tick (inc (:tick cache))]
      [(-> cache
           (assoc :tick tick)
           (update :hits inc)
           (assoc-in [:entries k :tick] tick))
       (get-in (:entries cache) [k :val])])
    [(update cache :misses inc) nil]))

(defn cache-put
  "Associate `k` with `v`, evicting the LRU entry when full."
  [cache k v]
  (let [tick (inc (:tick cache))]
    (if (contains? (:entries cache) k)
      (-> cache
          (assoc :tick tick)
          (assoc-in [:entries k] {:val v :tick tick}))
      (let [entries (if (>= (count (:entries cache)) (:capacity cache))
                      (let [lru (first (apply min-key
                                              (fn [[_ e]] (:tick e))
                                              (:entries cache)))]
                        (dissoc (:entries cache) lru))
                      (:entries cache))]
        (-> cache
            (assoc :tick tick)
            (assoc :entries (assoc entries k {:val v :tick tick})))))))

(defn memoize-lru
  "Wrap single-arg fn `f` with an LRU cache of `size` entries.
  Returns `{:fn f :info (fn [] ...)}`."
  [size f]
  (let [cache (atom (make-lru size))]
    {:fn (fn [x]
           (let [[c v] (cache-get @cache x)]
             (reset! cache c)
             (if (some? v)
               v
               (let [result (f x)]
                 (swap! cache cache-put x result)
                 result))))
     :info (fn [] (cache-info @cache))}))
