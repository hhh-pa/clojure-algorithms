(ns clojure-algorithms.maths.is-square-free
  "References: wikipedia:square free number
psf/black : True
ruff : True")

(defn- type-name
  "Python-style type name used in the not-iterable error message."
  [x]
  (cond (nil? x) "NoneType"
        (boolean? x) "bool"
        (integer? x) "int"
        (number? x) "float"
        :else (.getSimpleName (class x))))

(defn is-square-free
  "Return true when the given collection of factors contains no repetition,
  i.e. it is square free. Throws :TypeError for non-iterable input."
  [factors]
  (if (or (sequential? factors)
          (string? factors)
          (set? factors)
          (map? factors))
    (= (count (set factors)) (count factors))
    (throw (ex-info (str "'" (type-name factors) "' object is not iterable")
                    {:type :TypeError}))))
