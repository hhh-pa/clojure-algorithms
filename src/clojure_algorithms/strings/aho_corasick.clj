(ns clojure-algorithms.strings.aho-corasick
  "Aho-Corasick multi-keyword search automaton.
  Ported from TheAlgorithms/Python.")

(defn- find-next-state
  "State reached from `current` on `ch`, or nil."
  [states current ch]
  (get-in states [current :next ch]))

(defn- add-keyword
  "Adds `keyword` to the trie `states`. Returns updated states."
  [states keyword]
  (let [[states2 current]
        (reduce (fn [[sts cur] ch]
                  (if-let [nxt (find-next-state sts cur ch)]
                    [sts nxt]
                    (let [idx (count sts)]
                      [(-> sts
                           (conj {:value ch :next {} :fail 0 :output []})
                           (assoc-in [cur :next ch] idx))
                       idx])))
                [states 0] keyword)]
    (update-in states2 [current :output] conj keyword)))

(defn- set-fail-transitions
  "Computes failure links (BFS) for the trie. Returns updated states."
  [states]
  (let [initial (mapv (fn [[_ s]] s) (get-in states [0 :next]))
        states2 (reduce (fn [sts s] (assoc-in sts [s :fail] 0))
                        states initial)]
    (loop [sts states2 queue (into clojure.lang.PersistentQueue/EMPTY initial)]
      (if (empty? queue)
        sts
        (let [r (peek queue)
              queue2 (pop queue)
              [sts2 queue3]
              (reduce (fn [[s q] [_ child]]
                        (let [st (loop [st (get-in s [r :fail])]
                                   (if (or (find-next-state
                                            s st (get-in s [child :value]))
                                           (zero? st))
                                     st
                                     (recur (get-in s [st :fail]))))
                              fail (or (find-next-state
                                        s st (get-in s [child :value])) 0)]
                          [(-> s
                               (assoc-in [child :fail] fail)
                               (update-in [child :output] into
                                          (get-in s [fail :output])))
                           (conj q child)]))
                      [sts queue2]
                      (get-in sts [r :next]))]
          (recur sts2 queue3))))))

(defn build-automaton
  "Builds an Aho-Corasick automaton (states vector) for `keywords`."
  [keywords]
  (set-fail-transitions
   (reduce add-keyword [{:value "" :next {} :fail 0 :output []}] keywords)))

(defn search-in
  "Searches `text` with automaton `states`. Returns {keyword [indices]}."
  [states text]
  (loop [i 0 current 0 result {}]
    (if (>= i (count text))
      result
      (let [ch (nth text i)
            cur (loop [st current]
                  (if (or (find-next-state states st ch) (zero? st))
                    st
                    (recur (get-in states [st :fail]))))
            nxt (find-next-state states cur ch)]
        (if (nil? nxt)
          (recur (inc i) 0 result)
          (recur (inc i) nxt
                 (reduce (fn [r kw]
                           (update r kw (fnil conj []) (- i (count kw) -1)))
                         result
                         (get-in states [nxt :output]))))))))
