(ns clojure-algorithms.maths.points-are-collinear-3d
  "Check if three points are collinear in 3D.

  Three points form a triangle; the area of that triangle determines whether
  the points are collinear. Two vectors with the same initial point are built
  from the three points, then their cross-product is computed. If the cross
  vector is (0, 0, 0) the points are collinear.

  Read More:
      https://math.stackexchange.com/a/1951650")

(defn create-vector
  "Returns the vector from `end-point1` to `end-point2` as [x y z]."
  [end-point1 end-point2]
  (mapv - end-point2 end-point1))

(defn get-3d-vectors-cross
  "Returns the cross product of the 3D vectors `ab` and `ac` as [x y z].
  See https://en.wikipedia.org/wiki/Cross_product"
  [ab ac]
  (let [[abx aby abz] ab
        [acx acy acz] ac]
    [(- (* aby acz) (* abz acy))
     (- (* abz acx) (* abx acz))
     (- (* abx acy) (* aby acx))]))

(defn- round-to
  "Rounds `x` to `ndigits` decimals with round-half-even on the exact binary
  value, matching Python's round()."
  [x ndigits]
  (let [bd (if (integer? x)
             (BigDecimal. ^java.math.BigInteger (biginteger x))
             (BigDecimal. ^double (double x)))]
    (.setScale bd ndigits java.math.RoundingMode/HALF_EVEN)))

(defn is-zero-vector
  "Returns true if `vector-v` rounds to [0 0 0] at the given `accuracy`
  (number of decimals)."
  [vector-v accuracy]
  (every? #(zero? (round-to % accuracy)) vector-v))

(defn are-collinear
  "Returns true if the three 3D points `a`, `b` and `c` are collinear."
  ([a b c] (are-collinear a b c 10))
  ([a b c accuracy]
   (is-zero-vector (get-3d-vectors-cross (create-vector a b)
                                         (create-vector a c))
                   accuracy)))
