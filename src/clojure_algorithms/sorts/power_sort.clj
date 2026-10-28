(ns clojure-algorithms.sorts.power-sort
  "PowerSort: adaptive stable merge sort over natural runs with a
  power-based merge policy (Munro & Wild). Supports :key and :reverse.
  Ported from TheAlgorithms/Python.")

(defn find-run
  "Finds the run starting at `start` in vector `v` (comparing by `kf`).
  Descending runs are reversed. Returns [v run-end-exclusive]."
  ([v start end] (find-run v start end identity))
  ([v start end kf]
   (if (>= start (dec end))
     [v (inc start)]
     (let [run-asc? (not (neg? (compare (kf (nth v (inc start)))
                                        (kf (nth v start)))))
           run-end (if run-asc?
                     (loop [r (inc start)]
                       (if (and (< r end)
                                (not (neg? (compare (kf (nth v r))
                                                    (kf (nth v (dec r)))))))
                         (recur (inc r)) r))
                     (loop [r (inc start)]
                       (if (and (< r end)
                                (neg? (compare (kf (nth v r))
                                               (kf (nth v (dec r))))))
                         (recur (inc r)) r)))]
       (if run-asc?
         [v run-end]
         [(into (into (subvec v 0 start) (rseq (subvec v start run-end)))
                (subvec v run-end))
          run-end])))))

(defn node-power
  "Merge priority of two adjacent runs ([b1,n1] and [b2,n2]) in an
  array of `total-length`: smallest p with different binary prefixes."
  [total-length b1 n1 b2 n2]
  (let [a (+ (* 2 b1) n1)
        b (+ (* 2 b2) n2)
        two-n (* 2 total-length)]
    (loop [p 0]
      (if (= (quot (* a (bit-shift-left 1 p)) two-n)
             (quot (* b (bit-shift-left 1 p)) two-n))
        (recur (inc p))
        p))))

(defn merge-runs
  "Merges sorted runs [start1,end1) and [end1,end2) of `v` (by `kf`)."
  ([v start1 end1 end2] (merge-runs v start1 end1 end2 identity))
  ([v start1 end1 end2 kf]
   (let [left (subvec v start1 end1)
         right (subvec v end1 end2)
         merged (loop [l (seq left) r (seq right) acc (transient [])]
                  (cond
                    (nil? l) (persistent! (reduce conj! acc r))
                    (nil? r) (persistent! (reduce conj! acc l))
                    (pos? (compare (kf (first l)) (kf (first r))))
                    (recur l (next r) (conj! acc (first r)))
                    :else
                    (recur (next l) r (conj! acc (first l)))))]
     (into (into (subvec v 0 start1) merged) (subvec v end2)))))

(defn power-sort
  "Sorts `coll` with PowerSort. Options: :key key-fn, :reverse bool.
  Returns a sorted vector."
  [coll & {:keys [key reverse] :or {key nil reverse false}}]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (let [negate? (fn [x] (and reverse (number? x)))
            kf (cond
                 (and key reverse)
                 (fn [e] (let [k (key e)]
                           (if (number? k) (- k) k)))
                 key key
                 reverse (fn [e] (if (number? e) (- e) e))
                 :else identity)
            n (count v)
            non-numeric-reverse? (and reverse (not (number? (first v))))]
        (let [[sorted _]
              (loop [a v start 0 stack []]
                (if (>= start n)
                  ;; merge remaining runs
                  [(loop [b a st stack]
                     (if (<= (count st) 1)
                       b
                       (let [[s2 l2 _] (peek st)
                             st2 (pop st)
                             [s1 l1 _] (peek st2)
                             st3 (pop st2)
                             b2 (merge-runs b s1 (+ s1 l1) (+ s2 l2) kf)
                             p (if (empty? st3)
                                 0
                                 (let [[ps pl _] (peek st3)]
                                   (node-power n ps pl s1
                                               (- (+ s2 l2) s1))))]
                         (recur b2 (conj st3 [s1 (- (+ s2 l2) s1) p])))))
                   stack]
                  ;; find next run, merge by power
                  (let [[a2 run-end] (find-run a start n kf)
                        run-len (- run-end start)
                        p (if (empty? stack)
                            0
                            (let [[ps pl _] (peek stack)]
                              (node-power n ps pl start run-len)))
                        [a3 s3 start3 len3 p3]
                        (loop [b a2 st stack s start l run-len pw p]
                          (if (and (seq st) (>= (nth (peek st) 2) pw))
                            (let [[ps pl _] (peek st)
                                  st2 (pop st)
                                  b2 (merge-runs b ps (+ ps pl) (+ s l) kf)
                                  s2 ps
                                  l2 (- (+ s l) ps)
                                  pw2 (if (empty? st2)
                                        0
                                        (let [[qs ql _] (peek st2)]
                                          (node-power n qs ql s2 l2)))]
                              (recur b2 st2 s2 l2 pw2))
                            [b st s l pw]))]
                    (recur a3 (+ start3 len3)
                           (conj s3 [start3 len3 p3])))))]
          (if (and non-numeric-reverse? reverse)
            (vec (rseq sorted))
            sorted))))))
