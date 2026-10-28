(ns clojure-algorithms.searches.interpolation-search
  "Interpolation search over sorted integer collections.

  Port of TheAlgorithms/Python searches/interpolation_search.py. Both
  variants return the item index or nil when absent."

  (:refer-clojure :exclude []))

(defn- floordiv
  "Python-style floor division of `a` by `b`."
  [a b]
  (let [q (quot a b) r (rem a b)]
    (if (and (not (zero? r)) (not= (neg? a) (neg? b)))
      (dec q)
      q)))

(defn- probe
  "Interpolated probe index for `item` within [left, right] of `v`."
  [v item left right]
  (+ left (floordiv (* (- item (nth v left)) (- right left))
                    (- (nth v right) (nth v left)))))

(defn interpolation-search
  "Index of `item` in `sorted-coll`, or nil if absent."
  [sorted-coll item]
  (let [v (vec sorted-coll)]
    (loop [left 0 right (dec (count v))]
      (if (> left right)
        nil
        (if (= (nth v left) (nth v right))
          (when (= (nth v left) item) left)
          (let [point (probe v item left right)]
            (if (or (neg? point) (>= point (count v)))
              nil
              (let [current (nth v point)]
                (cond (= current item) point
                      (< point left) (recur point left)
                      (> point right) (recur right point)
                      (< item current) (recur left (dec point))
                      :else (recur (inc point) right))))))))))

(defn interpolation-search-by-recursion
  "Index of `item` within [left, right] of `sorted-coll`, or nil.

  With two arguments the whole collection is searched; `right` defaults
  to the last index."
  ([sorted-coll item] (interpolation-search-by-recursion sorted-coll item 0 nil))
  ([sorted-coll item left]
   (interpolation-search-by-recursion sorted-coll item left nil))
  ([sorted-coll item left right]
   (let [v (vec sorted-coll)
         right (if (nil? right) (dec (count v)) right)]
     (cond (> left right) nil
           (= (nth v left) (nth v right))
           (when (= (nth v left) item) left)
           :else (let [point (probe v item left right)]
                   (cond (or (neg? point) (>= point (count v))) nil
                         (= (nth v point) item) point
                         (< point left)
                         (interpolation-search-by-recursion v item point left)
                         (> point right)
                         (interpolation-search-by-recursion v item right left)
                         (> (nth v point) item)
                         (interpolation-search-by-recursion v item left (dec point))
                         :else
                         (interpolation-search-by-recursion v item (inc point)
                                                            right)))))))
