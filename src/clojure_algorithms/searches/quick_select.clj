(ns clojure-algorithms.searches.quick-select
  "Randomized quick select and median finding.

  Port of TheAlgorithms/Python searches/quick_select.py. Pivot choice is
  seedable via an optional long seed or java.util.Random argument; the
  returned values are deterministic regardless of the seed.")

(defn- ->rng
  "Coerce a long seed or Random to a java.util.Random."
  [seed-or-rng]
  (if (integer? seed-or-rng)
    (java.util.Random. (long seed-or-rng))
    seed-or-rng))

(defn- partition-by-pivot
  "Split `data` into [smaller equal greater] vectors around `pivot`."
  [data pivot]
  [(filterv #(< % pivot) data)
   (filterv #(= % pivot) data)
   (filterv #(> % pivot) data)])

(defn quick-select
  "Element at sorted position `index` (0-based) in `items`, or nil for
  an out-of-range index. Accepts an optional RNG seed."
  ([items index] (quick-select items index (java.util.Random.)))
  ([items index seed-or-rng]
   (let [v (vec items)]
     (when (and (<= 0 index) (< index (count v)))
       (let [rng (->rng seed-or-rng)
             pivot (nth v (.nextInt rng (count v)))
             [smaller equal larger] (partition-by-pivot v pivot)
             m (count smaller)
             c (count equal)]
         (cond (and (<= m index) (< index (+ m c))) pivot
               (> m index) (quick-select smaller index rng)
               :else (quick-select larger (- index (+ m c)) rng)))))))

(defn median
  "Middle value of `items` (mean of the two middles for even counts).
  Accepts an optional RNG seed."
  ([items] (median items (java.util.Random.)))
  ([items seed-or-rng]
   (let [v (vec items)
         rng (->rng seed-or-rng)
         mid (quot (count v) 2)]
     (if (odd? (count v))
       (quick-select v mid rng)
       (double (/ (+ (quick-select v (dec mid) rng)
                     (quick-select v mid rng))
                  2))))))
