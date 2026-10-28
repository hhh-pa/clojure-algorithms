(ns clojure-algorithms.project-euler.problem-068.sol1
  "Project Euler 68: magic 5-gon ring.

  Using the numbers 1 to 10, and depending on arrangements, it is
  possible to form 16- and 17-digit strings. What is the maximum
  16-digit string for a magic 5-gon ring?")

(defn- permutations
  "Lazily returns all permutations of coll, in the same order as
  Python's itertools.permutations."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      (list ())
      (mapcat (fn [i]
                (map #(cons (nth v i) %)
                     (permutations (concat (subvec v 0 i)
                                           (subvec v (inc i))))))
              (range (count v))))))

(defn generate-gon-ring
  "Generates a gon-side-gon ring from a permutation state. The
  permutation state is the ring, but every duplicate is removed."
  [gon-side perm]
  (let [perm (conj (vec perm) (nth perm 1))
        result (vec (concat (take 3 perm)
                            (repeat (- (* gon-side 3) 3) 0)))
        magic-number (if (< gon-side 5) 1 2)]
    (reduce (fn [res i]
              (-> res
                  (assoc (* 3 i) (nth perm (inc (* 2 i))))
                  (assoc (inc (* 3 i)) (nth res (dec (* 3 i))))
                  (assoc (+ 2 (* 3 i)) (nth perm (+ 2 (* 2 i))))))
            result
            (range 1 (+ (quot (count perm) 3) magic-number)))))

(defn is-magic-gon
  "Checks if the solution set is a magic n-gon ring: the first number
  must be the smallest number on the outer ring, and the sum of each
  3-number chunk must equal the same total."
  [numbers]
  (when (not= 0 (mod (count numbers) 3))
    (throw (IllegalArgumentException.
            "a gon ring should have a length that is a multiple of 3")))
  (if (not= (apply min (take-nth 3 numbers)) (first numbers))
    false
    (let [total (reduce + (take 3 numbers))]
      (every? #(= total (reduce + %)) (partition 3 (drop 3 numbers))))))

(defn solution
  "Finds the maximum number for a magic gon-side-gon ring. The gon-side
  parameter should be in the range [3, 5]."
  ([] (solution 5))
  ([gon-side]
   (when (or (< gon-side 3) (> gon-side 5))
     (throw (IllegalArgumentException. "gon_side must be in the range [3, 5]")))
   ;; Since it is 16 digits, we know 10 is on the outer ring.
   ;; Put the big numbers at the end so that they are never the first number.
   (let [small-numbers (vec (range (inc gon-side) 0 -1))
         big-numbers (vec (range (+ gon-side 2) (inc (* gon-side 2))))]
     (if-let [found (some (fn [perm]
                            (let [numbers (generate-gon-ring gon-side (vec perm))]
                              (when (is-magic-gon numbers) numbers)))
                          (permutations (concat small-numbers big-numbers)))]
       (parse-long (apply str found))
       (throw (IllegalArgumentException.
               (str "Magic " gon-side "-gon ring is impossible")))))))
