(ns clojure-algorithms.maths.special-numbers.proth-number
  "Calculate the nth Proth number
Source:
    https://handwiki.org/wiki/Proth_number")

(defn- pow-int
  "Integer power: base**exp computed with exact integer arithmetic."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn- floor-log2
  "Floor of the base-2 logarithm of the positive integer x."
  [x]
  (dec (.bitLength (java.math.BigInteger/valueOf (long x)))))

(defn- check-positive-int!
  "Throws :TypeError/:ValueError ex-infos unless v is an integer above 0."
  [v]
  (when-not (integer? v)
    (throw (ex-info (str "Input value of [number=" v "] must be an integer")
                    {:type :TypeError})))
  (when (< v 1)
    (throw (ex-info (str "Input value of [number=" v "] must be > 0")
                    {:type :ValueError}))))

(defn proth
  "Returns the nth Proth number. Indexing starts at 1, so (proth 1) is the
first Proth number, 3."
  [number]
  (check-positive-int! number)
  (if (<= number 2)
    (nth [3 5] (dec number))
    (let [block-index (+ 2 (floor-log2 (quot number 3)))]
      (loop [block 1, proth-list [3 5], increment 3]
        (if (>= block block-index)
          (nth proth-list (dec number))
          (recur (inc block)
                 (reduce (fn [lst _]
                           (conj lst (+' (pow-int 2 (inc block)) (peek lst))))
                         proth-list
                         (range increment))
                 (* 2 increment)))))))

(defn is-proth-number
  "Returns true if `number` is a Proth number."
  [number]
  (check-positive-int! number)
  (if (= number 1)
    false
    (let [[odd-part n] (loop [m (dec number), n 0]
                         (if (even? m)
                           (recur (quot m 2) (inc n))
                           [m n]))]
      (< odd-part (pow-int 2 n)))))
