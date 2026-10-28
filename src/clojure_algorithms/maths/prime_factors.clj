(ns clojure-algorithms.maths.prime-factors
  "python/black : True")

(defn- type-name
  "Python-style type name used in the comparison error message."
  [x]
  (cond (nil? x) "NoneType"
        (boolean? x) "bool"
        (string? x) "str"
        (map? x) "dict"
        (set? x) "set"
        (sequential? x) "list"
        :else (.getSimpleName (class x))))

(defn- check-number
  "Reject non-numeric input like Python's <= comparison does."
  [n]
  (when-not (number? n)
    (throw (ex-info (str "'<=' not supported between instances of 'int' and '"
                         (type-name n) "'")
                    {:type :TypeError}))))

(defn prime-factors
  "Return the prime factors of n as a vector, with multiplicity."
  [n]
  (check-number n)
  (loop [i 2
         m n
         factors []]
    (if (<= (* i i) m)
      (if (zero? (rem m i))
        (recur i (quot m i) (conj factors i))
        (recur (inc i) m factors))
      (if (> m 1)
        (conj factors m)
        factors))))

(defn- strip-factor
  "Divide m by i until it no longer divides m."
  [m i]
  (loop [m m]
    (if (zero? (rem m i))
      (recur (quot m i))
      m)))

(defn unique-prime-factors
  "Return the unique prime factors of n as a vector."
  [n]
  (check-number n)
  (loop [i 2
         m n
         factors []]
    (if (<= (* i i) m)
      (if (zero? (rem m i))
        (recur (inc i) (strip-factor m i) (conj factors i))
        (recur (inc i) m factors))
      (if (> m 1)
        (conj factors m)
        factors))))
