(ns clojure-algorithms.searches.exponential-search
  "Exponential search over ascending-sorted collections.

  Port of TheAlgorithms/Python searches/exponential_search.py: grow a
  bound exponentially past the target, then binary-search the window.")

(defn- ensure-sorted
  "Throw when `coll` is not sorted in ascending order."
  [coll]
  (when (not= (vec coll) (vec (sort coll)))
    (throw (ex-info "sorted_collection must be sorted in ascending order"
                    {:collection (vec coll)}))))

(defn binary-search-by-recursion
  "Index of `item` within [left, right] of `sorted-coll`, or -1.

  With two arguments the whole collection is searched; `right` defaults
  to the last index."
  ([sorted-coll item] (binary-search-by-recursion sorted-coll item 0 nil))
  ([sorted-coll item left] (binary-search-by-recursion sorted-coll item left nil))
  ([sorted-coll item left right]
   (let [v (vec sorted-coll)]
     (ensure-sorted v)
     (let [right (if (nil? right) (dec (count v)) right)]
       (letfn [(search [l r]
                 (if (< r l)
                   -1
                   (let [mid (+ l (quot (- r l) 2))
                         c (compare (nth v mid) item)]
                     (cond (zero? c) mid
                           (pos? c) (search l (dec mid))
                           :else (search (inc mid) r)))))]
         (search left right))))))

(defn exponential-search
  "Index of `item` in ascending `sorted-coll`, or -1 if absent."
  [sorted-coll item]
  (let [v (vec sorted-coll)]
    (ensure-sorted v)
    (cond (empty? v) -1
          (= (first v) item) 0
          :else (let [bound (loop [bound 1]
                              (if (and (< bound (count v))
                                       (neg? (compare (nth v bound) item)))
                                (recur (* bound 2))
                                bound))]
                  (binary-search-by-recursion v item (quot bound 2)
                                              (min bound (dec (count v))))))))
