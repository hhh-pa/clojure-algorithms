(ns clojure-algorithms.strings.suffix-automaton
  "Suffix automaton: minimal DFA of all substrings.
  See https://cp-algorithms.com/string/suffix-automaton.html
  Ported from TheAlgorithms/Python.")

(defn- extend-sam
  "Appends `ch` to automaton `{:states [...] :last i}`."
  [{:keys [states last] :as sam} ch]
  (let [curr (count states)
        states2 (conj states {:length (inc (:length (nth states last)))
                              :link 0 :next {}})
        [states3 prev]
        (loop [sts states2 p last]
          (if (and (not= p -1)
                   (not (contains? (:next (nth sts p)) ch)))
            (recur (assoc-in sts [p :next ch] curr)
                   (:link (nth sts p)))
            [sts p]))]
    (if (= prev -1)
      {:states (assoc-in states3 [curr :link] 0) :last curr}
      (let [nxt (get-in states3 [prev :next ch])]
        (if (= (inc (:length (nth states3 prev)))
               (:length (nth states3 nxt)))
          {:states (assoc-in states3 [curr :link] nxt) :last curr}
          (let [clone (count states3)
                states4 (conj states3
                              {:length (inc (:length (nth states3 prev)))
                               :link (:link (nth states3 nxt))
                               :next (:next (nth states3 nxt))})
                states5 (loop [sts states4 p prev]
                          (if (and (not= p -1)
                                   (= (get-in sts [p :next ch]) nxt))
                            (recur (assoc-in sts [p :next ch] clone)
                                   (:link (nth sts p)))
                            sts))]
            {:states (-> states5
                         (assoc-in [nxt :link] clone)
                         (assoc-in [curr :link] clone))
             :last curr}))))))

(defn build-suffix-automaton
  "Builds the suffix automaton for non-empty string `s`.
  Throws IllegalArgumentException on empty input."
  [s]
  (when (empty? s)
    (throw (IllegalArgumentException. "Input string must not be empty.")))
  (reduce extend-sam {:states [{:length 0 :link -1 :next {}}] :last 0} s))

(defn sam-contains?
  "True when `pattern` is a substring recognized by automaton `sam`."
  [sam pattern]
  (loop [curr 0 chars (seq pattern)]
    (if (nil? chars)
      true
      (let [nxt (get-in (:states sam) [curr :next (first chars)])]
        (if (nil? nxt)
          false
          (recur nxt (next chars)))))))

(defn count-distinct-substrings
  "Number of distinct substrings recognized by automaton `sam`."
  [sam]
  (reduce + (map (fn [st]
                   (- (:length st)
                      (:length (nth (:states sam) (:link st)))))
                 (rest (:states sam)))))

(defn count-occurrences
  "Occurrences of `pattern` in the automaton's original string `s`."
  [sam s pattern]
  (let [curr (loop [c 0 chars (seq pattern)]
               (cond
                 (nil? chars) c
                 (nil? (get-in (:states sam) [c :next (first chars)]))
                 ::missing
                 :else (recur (get-in (:states sam) [c :next (first chars)])
                              (next chars))))]
    (if (= curr ::missing)
      0
      (let [n (count (:states sam))
            order (sort-by #(:length (nth (:states sam) %)) > (range n))
            occ (loop [o (vec (repeat n 0)) t 0 chars (seq s)]
                  (if (nil? chars)
                    o
                    (let [t2 (get-in (:states sam) [t :next (first chars)])]
                      (recur (assoc o t2 1) t2 (next chars)))))
            occ2 (reduce (fn [o idx]
                           (let [link (:link (nth (:states sam) idx))]
                             (if (= link -1) o
                                 (update o link + (nth o idx)))))
                         occ order)]
        (nth occ2 curr)))))
