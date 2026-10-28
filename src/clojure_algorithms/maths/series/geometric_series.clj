(ns clojure-algorithms.maths.series.geometric-series
  "This is a pure Python implementation of the Geometric Series algorithm
https://en.wikipedia.org/wiki/Geometric_series
Run the doctests with the following command:
python3 -m doctest -v geometric_series.py
or
python -m doctest -v geometric_series.py
For manual testing run:
python3 geometric_series.py")

(defn- truthy-arg?
  [v]
  (cond (nil? v) false
        (false? v) false
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (coll? v) (not (empty? v))
        :else true))

(defn geometric-series
  "Return the Geometric Series of nth-term terms starting from start-term-a
with common ratio common-ratio-r.

  Returns an empty vector when any argument is zero-like, matching the
  upstream guard."
  [nth-term start-term-a common-ratio-r]
  (if-not (every? truthy-arg? [nth-term start-term-a common-ratio-r])
    []
    (mapv (fn [i]
            (if (zero? i)
              start-term-a
              (double (* start-term-a
                         (Math/pow (double common-ratio-r) i)))))
          (range (int nth-term)))))
