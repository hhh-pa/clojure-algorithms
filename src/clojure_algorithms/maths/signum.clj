(ns clojure-algorithms.maths.signum
  "Signum function -- https://en.wikipedia.org/wiki/Sign_function")

(defn- type-name
  "Python-style type name used in error messages."
  [x]
  (cond
    (string? x) "str"
    (boolean? x) "bool"
    (sequential? x) "list"
    (map? x) "dict"
    (nil? x) "NoneType"
    :else "unknown"))

(defn signum
  "Applies the signum function to the number:
-1 when negative, 0 when zero, 1 when positive."
  [num]
  (if (number? num)
    (cond (neg? num) -1
          (zero? num) 0
          :else 1)
    (throw (ex-info (str "'<' not supported between instances of '"
                         (type-name num) "' and 'int'")
                    {:type :TypeError}))))

(defn test-signum
  "Asserts the signum function over a range of inputs."
  []
  (assert (= (signum 5) 1))
  (assert (= (signum -5) -1))
  (assert (= (signum 0) 0))
  (assert (= (signum 10.5) 1))
  (assert (= (signum -10.5) -1))
  (assert (= (signum 1e-6) 1))
  (assert (= (signum -1e-6) -1))
  (assert (= (signum 123456789) 1))
  (assert (= (signum -123456789) -1))
  nil)
