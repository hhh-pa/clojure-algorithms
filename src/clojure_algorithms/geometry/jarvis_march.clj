(ns clojure-algorithms.geometry.jarvis-march
  "Convex hull via the Jarvis march (gift wrapping) algorithm.")

(defrecord Point [x y])

(defn- cross-product [o a b]
  (- (* (- (:x a) (:x o)) (- (:y b) (:y o)))
     (* (- (:y a) (:y o)) (- (:x b) (:x o)))))

(defn- point-on-segment? [p1 p2 point]
  (let [cross (- (* (- (:y point) (:y p1)) (- (:x p2) (:x p1)))
                 (* (- (:x point) (:x p1)) (- (:y p2) (:y p1))))]
    (and (<= (Math/abs (double cross)) 1e-9)
         (<= (min (:x p1) (:x p2)) (:x point) (max (:x p1) (:x p2)))
         (<= (min (:y p1) (:y p2)) (:y point) (max (:y p1) (:y p2))))))

(defn- leftmost-index [pts]
  (reduce (fn [best i]
            (let [p (nth pts i) b (nth pts best)]
              (if (or (< (:x p) (:x b))
                      (and (= (:x p) (:x b)) (< (:y p) (:y b))))
                i best)))
          0 (range (count pts))))

(defn- next-hull-index [pts current]
  (let [n (count pts)
        start (mod (inc current) n)
        start (loop [s start]
                (if (= s current) (recur (mod (inc s) n)) s))]
    (reduce (fn [nxt i]
              (if (or (= i current)
                      (<= (cross-product (nth pts current)
                                         (nth pts i) (nth pts nxt)) 0))
                nxt i))
            start (range n))))

(defn- valid-polygon? [hull]
  (let [n (count hull)]
    (boolean
     (some (fn [i]
             (> (Math/abs (double (cross-product (nth hull i)
                                                 (nth hull (mod (inc i) n))
                                                 (nth hull (mod (+ i 2) n)))))
                1e-9))
           (range n)))))

(defn jarvis-march
  "Returns the convex hull in counter-clockwise order, or [] when fewer
   than 3 non-collinear points are given."
  [points]
  (if (<= (count points) 2)
    []
    (let [pts (vec (distinct points))]
      (if (<= (count pts) 2)
        []
        (let [left (leftmost-index pts)
              hull (loop [hull [(nth pts left)] current left]
                     (let [nxt (next-hull-index pts current)]
                       (if (or (= nxt left) (= nxt current))
                         hull
                         (let [pt (nth pts nxt)
                               n (count hull)]
                           (recur
                            (if (and (> n 1)
                                     (point-on-segment? (nth hull (- n 2))
                                                        (nth hull (dec n)) pt))
                              (assoc hull (dec n) pt)
                              (conj hull pt))
                            nxt)))))]
          (if (<= (count hull) 2)
            []
            (let [trimmed (let [n (count hull)]
                            (if (point-on-segment? (nth hull (- n 2))
                                                   (nth hull (dec n))
                                                   (nth hull 0))
                              (pop hull) hull))]
              (if (or (<= (count trimmed) 2) (not (valid-polygon? trimmed)))
                [] (vec trimmed)))))))))
