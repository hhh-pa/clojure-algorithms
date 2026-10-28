(ns clojure-algorithms.data-structures.linked-list.is-palindrome
  "Port of data_structures/linked_list/is_palindrome.py.
  Three palindrome checks over {:val :next} chains.")

(defn make-node
  "Create a chain node with VAL and optional NEXT."
  ([val] (make-node val nil))
  ([val next] {:val val :next next}))

(defn chain-from-vec
  "Chain over the items of V."
  [v]
  (reduce (fn [nxt x] (make-node x nxt)) nil (reverse v)))

(defn chain->vec
  "Data vector of the chain."
  [head]
  (vec (take-while some? (map :val (iterate :next head)))))

(defn- second-half [items]
  (let [n (count items)]
    (subvec items (+ (quot n 2) (mod n 2)) n)))

(defn is-palindrome
  "Two-pointer palindrome check."
  [head]
  (let [items (chain->vec head)]
    (= (subvec items 0 (quot (count items) 2))
       (vec (reverse (second-half items))))))

(defn is-palindrome-stack
  "Stack-based palindrome check."
  [head]
  (let [items (chain->vec head)]
    (if (< (count items) 2)
      true
      (let [slow-idx (loop [slow 0 fast 0]
                       (if (and (< fast (count items))
                                (< (inc fast) (count items)))
                         (recur (inc slow) (+ fast 2))
                         slow))
            stack (subvec items slow-idx)]
        (= (seq (reverse stack))
           (take (count stack) items))))))

(defn is-palindrome-dict
  "Position-map palindrome check."
  [head]
  (let [items (chain->vec head)]
    (if (< (count items) 2)
      true
      (let [positions (reduce (fn [d [pos v]]
                                (update d v (fnil conj []) pos))
                              {} (map-indexed vector items))
            checksum (dec (count items))]
        (loop [vals (vals positions) middle 0]
          (cond
            (> middle 1) false
            (empty? vals) true
            (odd? (count (first vals))) (recur (rest vals) (inc middle))
            :else (let [v (first vals)]
                    (if (every? (fn [[step i]]
                                  (= (+ (nth v i)
                                        (nth v (- (count v) 1 step)))
                                     checksum))
                                (map-indexed vector (range (count v))))
                      (recur (rest vals) middle)
                      false))))))))
