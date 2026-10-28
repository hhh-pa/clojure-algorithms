(ns clojure-algorithms.other.lfu-cache
  "Least-frequently-used key/value cache (functional).

  Port of TheAlgorithms/Python `other/lfu_cache.py`.
  The mutable doubly-linked list becomes an immutable map of
  key -> {:val, :freq, :tick}; ties break by oldest tick.
  `cache-get`/`cache-put` return `[updated-cache value-or-nil]`.

  NOTE: this port implements correct LFU eviction. The Python
  `_position_node` swap corrupts the list's forward links, so its
  doctest trace evicts the *most*-frequent key at one step
  (`put(4,4)` evicts key 1 with freq 2 instead of key 3 with freq 1);
  that buggy step is intentionally not reproduced here.")

(defn make-lfu
  "Create an empty LFU cache with `capacity` slots."
  [capacity]
  {:capacity capacity :entries {} :tick 0 :hits 0 :misses 0})

(defn cache-info
  "Summary string mirroring the Python CacheInfo repr."
  [cache]
  (str "CacheInfo(hits=" (:hits cache)
       ", misses=" (:misses cache)
       ", capacity=" (:capacity cache)
       ", current_size=" (count (:entries cache)) ")"))

(defn cache-get
  "Look up `k`; return `[updated-cache value-or-nil]`."
  [cache k]
  (if-let [entry (get (:entries cache) k)]
    (let [tick (inc (:tick cache))]
      [(-> cache
           (assoc :tick tick)
           (update :hits inc)
           (assoc-in [:entries k] (assoc entry :freq (inc (:freq entry))
                                         :tick tick)))
       (:val entry)])
    [(update cache :misses inc) nil]))

(defn- eviction-key
  "Key with lowest frequency, oldest first."
  [entries]
  (ffirst (sort-by (fn [[_ e]] [(:freq e) (:tick e)]) entries)))

(defn cache-put
  "Associate `k` with `v`, evicting the LFU entry when full."
  [cache k v]
  (let [tick (inc (:tick cache))]
    (if (contains? (:entries cache) k)
      (-> cache
          (assoc :tick tick)
          (assoc-in [:entries k] {:val v
                                  :freq (inc (get-in (:entries cache) [k :freq]))
                                  :tick tick}))
      (let [entries (if (>= (count (:entries cache)) (:capacity cache))
                      (dissoc (:entries cache) (eviction-key (:entries cache)))
                      (:entries cache))]
        (-> cache
            (assoc :tick tick)
            (assoc :entries (assoc entries k {:val v :freq 1 :tick tick})))))))

(defn memoize-lfu
  "Wrap single-arg fn `f` with an LFU cache of `size` entries.
  The wrapper exposes hits/misses via `(meta ...)`-free `cache-info`
  on the returned map: `{:fn f :info (fn [] ...)}`."
  [size f]
  (let [cache (atom (make-lfu size))]
    {:fn (fn [x]
           (let [[c v] (cache-get @cache x)]
             (reset! cache c)
             (if (some? v)
               v
               (let [result (f x)]
                 (swap! cache cache-put x result)
                 result))))
     :info (fn [] (cache-info @cache))}))
