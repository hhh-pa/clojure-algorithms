(ns clojure-algorithms.data-structures.stacks.kth-next-greater-element
  "Port of data_structures/stacks/kth_next_greater_element.py.
  kth next-greater-element for every position via k stacks.")

(def test-k 10)
(def test-array (vec (range 10000)))
(def expected-answers
  (vec (concat (map #(+ % test-k) (range (- 10000 test-k)))
               (repeat test-k nil))))

(defn find-kth-next-greater-element
  "Vector of each element's KTH-ORD next greater element (nil if none)."
  [array kth-ord]
  (when-not (and (integer? kth-ord) (>= kth-ord 1))
    (throw (ex-info "kth_ord must be a positive integer."
                    {:type :value-error})))
  (let [n (count array)
        result (vec (repeat n nil))]
    (if (>= kth-ord n)
      result
      (loop [idx 0
             stacks (vec (repeat (inc kth-ord) []))
             result result]
        (if (>= idx n)
          result
          (let [[stacks result]
                (loop [stacks stacks result result]
                  (let [top (peek (nth stacks kth-ord))]
                    (if (and top (< (first top) (nth array idx)))
                      (recur (update stacks kth-ord pop)
                             (assoc result (second top) (nth array idx)))
                      [stacks result])))
                stacks
                (loop [ord (dec kth-ord) stacks stacks]
                  (if (< ord 1)
                    stacks
                    (let [stacks
                          (loop [stacks stacks]
                            (let [top (peek (nth stacks ord))]
                              (if (and top (< (first top) (nth array idx)))
                                (recur (-> stacks
                                           (update ord pop)
                                           (update 0 conj top)))
                                stacks)))
                          stacks
                          (loop [stacks stacks]
                            (if (empty? (nth stacks 0))
                              stacks
                              (recur (-> stacks
                                         (update 0 pop)
                                         (update (inc ord) conj
                                                 (peek (nth stacks 0)))))))]
                      (recur (dec ord) stacks))))
                stacks (if (>= (- n 1 idx) kth-ord)
                         (update stacks 1 conj [(nth array idx) idx])
                         stacks)]
            (recur (inc idx) stacks result)))))))
