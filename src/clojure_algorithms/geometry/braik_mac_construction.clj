(ns clojure-algorithms.geometry.braik-mac-construction
  "Conic through 5 points via the Braikenridge-Maclaurin construction.")

(defrecord Point [x y])

(defn make-point
  "Builds a Point, validating numeric coordinates."
  ([] (->Point 0.0 0.0))
  ([x y]
   (when-not (number? x)
     (throw (IllegalArgumentException. "x must be an int or float numeric value")))
   (when-not (number? y)
     (throw (IllegalArgumentException. "y must be an int or float numeric value")))
   (->Point x y)))

(defn determinant
  "Determinant of a square matrix (vector of row vectors) via Gaussian
   elimination with partial pivoting."
  [rows]
  (let [n (count rows)
        a (into-array (map double-array rows))]
    (loop [col 0 sign 1.0]
      (if (>= col n)
        (reduce * sign (map #(aget ^doubles (aget a %) %) (range n)))
        (let [pivot (loop [r col best col]
                      (if (>= r n) best
                          (recur (inc r)
                                 (if (> (Math/abs (aget ^doubles (aget a r) col))
                                        (Math/abs (aget ^doubles (aget a best) col)))
                                   r best))))]
          (if (zero? (aget ^doubles (aget a pivot) col))
            0.0
            (do
              (when (not= pivot col)
                (let [tmp (aget a col)]
                  (aset a col (aget a pivot))
                  (aset a pivot tmp)))
              (doseq [r (range (inc col) n)]
                (let [factor (/ (aget ^doubles (aget a r) col)
                                (aget ^doubles (aget a col) col))]
                  (doseq [c (range col n)]
                    (aset ^doubles (aget a r) c
                          (- (aget ^doubles (aget a r) c)
                             (* factor (aget ^doubles (aget a col) c)))))))
              (recur (inc col) (if (= pivot col) sign (- sign))))))))))

(defn conic-coefficients
  "Returns [a b c d e f] of the conic a*x^2+b*xy+c*y^2+d*x+e*y+f = 0
   through the 5 given points."
  [points]
  (when (or (not= (count points) 5) (not-every? #(instance? Point %) points))
    (throw (IllegalArgumentException. "Array must be 5 point objects.")))
  (let [row (fn [fns] (mapv (fn [{:keys [x y]}] (mapv #(% x y) fns)) points))
        sq (fn [x _] (* x x)) xy (fn [x y] (* x y))
        ys (fn [_ y] (* y y)) xx (fn [x _] x) yy (fn [_ y] y)
        one (fn [_ _] 1)]
    [(determinant (row [xy ys xx yy one]))
     (- (determinant (row [sq ys xx yy one])))
     (determinant (row [sq xy xx yy one]))
     (- (determinant (row [sq xy ys yy one])))
     (determinant (row [sq xy ys xx one]))
     (- (determinant (row [sq xy ys xx yy])))]))

(defn conic-generate
  "Renders the conic equation through 5 points as a string."
  [points]
  (let [[a b c d e f] (conic-coefficients points)]
    (format "0 = %+.2g X**2 %+.2g XY %+.2g Y**2 %+.2g X %+.2g Y %+.2g"
            (double a) (double b) (double c)
            (double d) (double e) (double f))))
