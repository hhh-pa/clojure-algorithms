(ns clojure-algorithms.maths.shoelace-area
  "Shoelace formula (Gauss's area formula) for polygon area.

  The function accepts an iterable of (x, y) pairs and returns the polygon area
  as a non-negative float.

  References:
  - https://en.wikipedia.org/wiki/Shoelace_formula")

(defn- as-double
  "Coerces a coordinate to double, like Python's float()."
  [x]
  (cond
    (number? x) (double x)
    (string? x) (Double/parseDouble x)
    :else (throw (IllegalArgumentException. "not a number"))))

(defn- parse-point
  "Parses one (x, y) pair into a pair of doubles."
  [p]
  (if (and (sequential? p) (= 2 (count p)))
    (let [[x y] p] [(as-double x) (as-double y)])
    (throw (IllegalArgumentException. "not a pair"))))

(defn shoelace-area
  "Computes the area of a simple polygon using the shoelace formula.
  `points` is an iterable of (x, y) coordinate pairs; the polygon is
  assumed closed (the last point wraps to the first). Returns a
  non-negative float."
  [points]
  (let [pts (vec points)]
    (when (< (count pts) 3)
      (throw (ex-info "At least 3 points are required to form a polygon"
                      {:type :ValueError})))
    (let [coords (try
                   (mapv parse-point pts)
                   (catch Exception _
                     (throw (ex-info "points must be an iterable of (x, y) numeric pairs"
                                     {:type :TypeError}))))
          n (count coords)]
      (/ (Math/abs (double (reduce + (map (fn [i]
                                            (let [[[x1 y1] [x2 y2]]
                                                  [(nth coords i)
                                                   (nth coords (mod (inc i) n))]]
                                              (- (* x1 y2) (* x2 y1))))
                                          (range n)))))
         2.0))))
