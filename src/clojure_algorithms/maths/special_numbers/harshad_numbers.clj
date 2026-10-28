(ns clojure-algorithms.maths.special-numbers.harshad-numbers
  "A harshad number (or more specifically an n-harshad number) is a number that's
divisible by the sum of its digits in some given base n.
Reference: https://en.wikipedia.org/wiki/Harshad_number")

(def ^:private digit-chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn- check-base!
  "Throws an ex-info with :type :ValueError unless base is within 2..36."
  [base]
  (when (or (< base 2) (> base 36))
    (throw (ex-info "'base' must be between 2 and 36 inclusive"
                    {:type :ValueError}))))

(defn int-to-base
  "Converts the non-negative integer `number` to its string representation in
`base` (2 to 36). Throws an ex-info with :type :ValueError when `base` is out
of range or `number` is negative."
  [number base]
  (check-base! base)
  (when (neg? number)
    (throw (ex-info "number must be a positive integer" {:type :ValueError})))
  (if (zero? number)
    "0"
    (loop [n number, chars ()]
      (if (zero? n)
        (apply str chars)
        (recur (quot n base)
               (conj chars (nth digit-chars (rem n base))))))))

(defn sum-of-digits
  "Sums the digit values of `num` expressed in `base` and returns that sum
rendered as a string in `base`."
  [num base]
  (check-base! base)
  (->> (int-to-base num base)
       (map #(Integer/parseInt (str %) base))
       (reduce + 0)
       (#(int-to-base % base))))

(defn harshad-numbers-in-base
  "Returns a vector of the string representations (in `base`) of all Harshad
numbers below `limit`. Returns an empty vector when `limit` is negative."
  [limit base]
  (check-base! base)
  (if (neg? limit)
    []
    (mapv int-to-base
          (filter #(zero? (rem % (Integer/parseInt (sum-of-digits % base) base)))
                  (range 1 limit))
          (repeat base))))

(defn is-harshad-number-in-base
  "Returns true if `num` expressed in `base` is a Harshad number, i.e. it is
divisible by the sum of its digits. Returns false for negative numbers."
  [num base]
  (check-base! base)
  (if (neg? num)
    false
    (let [n (Integer/parseInt (int-to-base num base) base)
          d (Integer/parseInt (sum-of-digits num base) base)]
      (zero? (rem n d)))))
