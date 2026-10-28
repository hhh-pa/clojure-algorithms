(ns clojure-algorithms.project-euler.problem-124.sol1
  "Project Euler Problem 124: ordered radicals.

  Loops over sorted rads and generates all numbers n for each rad. Keeps
  track of the total number of n, and when k falls inside some rad it sorts
  all n for it and picks the associated one.")

(defn generate-primes
  "Calculates the list of primes up to and including n."
  [n]
  (if (< n 2)
    []
    (let [primes (boolean-array (inc n) true)]
      (aset primes 0 false)
      (aset primes 1 false)
      (doseq [i (range 2 (inc (int (Math/sqrt (inc n)))))]
        (when (aget primes i)
          (loop [j (* i i)]
            (when (<= j n)
              (aset primes j false)
              (recur (+ j i))))))
      (vec (filter #(aget primes %) (range 2 (inc n)))))))

(defn generate-n
  "Generates all numbers n that can be constructed out of factors, with any
  multiplicity, but that do not exceed n-max. Returns them as a set."
  [factors n-max n]
  (if (empty? factors)
    #{}
    (let [fac (first factors)
          rest-factors (rest factors)]
      (loop [n n
             acc #{}]
        (if (> n n-max)
          acc
          (recur (* n fac)
                 (into (conj acc n)
                       (generate-n rest-factors n-max n))))))))

(defn generate-rads
  "Generates all rads and associated factors, e.g. rad is the product of its
  factors. Returns a map from rad to its factor vector."
  [factors-all n-max n factors-prev]
  (let [factors-all (vec factors-all)]
    (loop [i 0
           acc {}]
      (if (>= i (count factors-all))
        acc
        (let [f (nth factors-all i)
              n-new (* n f)]
          (if (> n-new n-max)
            acc
            (let [factors-new (conj (vec factors-prev) f)]
              (recur (inc i)
                     (merge acc
                            {n-new factors-new}
                            (generate-rads (subvec factors-all (inc i))
                                           n-max
                                           n-new
                                           factors-new))))))))))

(defn solution
  "Loops over sorted rads and generates all numbers n for each rad. Keeps
  track of the total number of n, and when k falls inside some rad, sorts
  all n for it and picks the associated n."
  ([] (solution 100000 10000))
  ([n-max k]
   (if (= k 1)
     1
     (let [primes (generate-primes n-max)
           rads-d (generate-rads primes n-max 1 [])
           rads (sort (keys rads-d))]
       (loop [rads rads
              tot 1]
         (if (empty? rads)
           -1
           (let [r (first rads)
                 facts (get rads-d r)
                 res (sort (generate-n facts n-max r))
                 res-len (count res)]
             (if (>= (+ tot res-len) k)
               (nth res (- k tot 1))
               (recur (rest rads) (+ tot res-len))))))))))
