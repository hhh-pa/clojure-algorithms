(ns clojure-algorithms.searches.binary-search
  "Binary search algorithms: bisection, insertion, and search variants.

  Port of TheAlgorithms/Python searches/binary_search.py. The mutating
  insort helpers return new vectors instead of modifying in place; all
  search functions return -1 (or [] for the duplicates variant) when the
  item is absent and throw when the input is not ascending-sorted.")

(defn- ensure-sorted
  "Throw when `coll` is not sorted in ascending order."
  [coll]
  (when (some true? (map #(pos? (compare %1 %2)) coll (rest coll)))
    (throw (ex-info "sorted_collection must be sorted in ascending order"
                    {:collection (vec coll)}))))

(defn bisect-left
  "Index of the first element >= item in ascending `sorted-coll`.

  Only indices in [lo, hi) are considered; a negative hi means the end."
  ([sorted-coll item] (bisect-left sorted-coll item 0 -1))
  ([sorted-coll item lo] (bisect-left sorted-coll item lo -1))
  ([sorted-coll item lo hi]
   (let [v (vec sorted-coll)
         hi (if (neg? hi) (count v) hi)]
     (loop [lo lo hi hi]
       (if (>= lo hi)
         lo
         (let [mid (+ lo (quot (- hi lo) 2))]
           (if (neg? (compare (nth v mid) item))
             (recur (inc mid) hi)
             (recur lo mid))))))))

(defn bisect-right
  "Index of the first element > item in ascending `sorted-coll`.

  Only indices in [lo, hi) are considered; a negative hi means the end."
  ([sorted-coll item] (bisect-right sorted-coll item 0 -1))
  ([sorted-coll item lo] (bisect-right sorted-coll item lo -1))
  ([sorted-coll item lo hi]
   (let [v (vec sorted-coll)
         hi (if (neg? hi) (count v) hi)]
     (loop [lo lo hi hi]
       (if (>= lo hi)
         lo
         (let [mid (+ lo (quot (- hi lo) 2))]
           (if (pos? (compare (nth v mid) item))
             (recur lo mid)
             (recur (inc mid) hi))))))))

(defn insort-left
  "Return a new vector with `item` inserted before equal elements."
  ([sorted-coll item] (insort-left sorted-coll item 0 -1))
  ([sorted-coll item lo] (insort-left sorted-coll item lo -1))
  ([sorted-coll item lo hi]
   (let [v (vec sorted-coll)
         i (bisect-left v item lo hi)]
     (vec (concat (subvec v 0 i) [item] (subvec v i))))))

(defn insort-right
  "Return a new vector with `item` inserted after equal elements."
  ([sorted-coll item] (insort-right sorted-coll item 0 -1))
  ([sorted-coll item lo] (insort-right sorted-coll item lo -1))
  ([sorted-coll item lo hi]
   (let [v (vec sorted-coll)
         i (bisect-right v item lo hi)]
     (vec (concat (subvec v 0 i) [item] (subvec v i))))))

(defn binary-search
  "Index of the leftmost occurrence of `item`, or -1 if absent."
  [sorted-coll item]
  (let [v (vec sorted-coll)]
    (ensure-sorted v)
    (loop [left 0 right (dec (count v)) result -1]
      (if (> left right)
        result
        (let [mid (+ left (quot (- right left) 2))
              c (compare (nth v mid) item)]
          (cond (zero? c) (recur left (dec mid) mid)
                (pos? c) (recur left (dec mid) result)
                :else (recur (inc mid) right result)))))))

(defn binary-search-std-lib
  "Index of `item` via `bisect-left`, or -1 if absent."
  [sorted-coll item]
  (let [v (vec sorted-coll)]
    (ensure-sorted v)
    (let [index (bisect-left v item)]
      (if (and (< index (count v)) (= (nth v index) item))
        index
        -1))))

(defn binary-search-with-duplicates
  "Vector of every index holding `item`, or [] if absent."
  [sorted-coll item]
  (let [v (vec sorted-coll)]
    (ensure-sorted v)
    (let [left (bisect-left v item)
          right (bisect-right v item)]
      (if (or (>= left (count v)) (not= (nth v left) item))
        []
        (vec (range left right))))))

(defn binary-search-by-recursion
  "Index of the leftmost occurrence of `item` within [left, right],
  or -1 if absent. A negative right means the last index."
  ([sorted-coll item] (binary-search-by-recursion sorted-coll item 0 -1))
  ([sorted-coll item left] (binary-search-by-recursion sorted-coll item left -1))
  ([sorted-coll item left right]
   (let [v (vec sorted-coll)]
     (ensure-sorted v)
     (let [right (if (neg? right) (dec (count v)) right)]
       (letfn [(search [l r]
                 (if (> l r)
                   -1
                   (let [mid (+ l (quot (- r l) 2))
                         c (compare (nth v mid) item)]
                     (cond (zero? c) (let [found (search l (dec mid))]
                                       (if (= found -1) mid found))
                           (pos? c) (search l (dec mid))
                           :else (search (inc mid) r)))))]
         (search left right))))))

(defn exponential-search
  "Index of `item` found by exponential bound growth plus binary search,
  or -1 if absent."
  [sorted-coll item]
  (let [v (vec sorted-coll)]
    (ensure-sorted v)
    (let [bound (loop [bound 1]
                  (if (and (< bound (count v))
                           (neg? (compare (nth v bound) item)))
                    (recur (* bound 2))
                    bound))]
      (binary-search-by-recursion v item (quot bound 2)
                                  (min bound (dec (count v)))))))
