(ns clojure-algorithms.other.dancing-links
  "Exact cover via Knuth's Algorithm X (Dancing Links).

  Port of TheAlgorithms/Python `other/dancing_links.py`.
  The mutable doubly-linked structure is replaced by an idiomatic
  functional Algorithm X search over sets; `solve` keeps the same
  contract: universe + subsets in, list of exact covers out.")

(defn- choose-column
  "Pick the universe element covered by the fewest subsets (MRV heuristic)."
  [universe subsets]
  (apply min-key (fn [u] (count (filter #(contains? % u) subsets))) universe))

(defn- search
  [universe subsets solution]
  (if (empty? universe)
    [(vec solution)]
    (let [col (choose-column universe subsets)
          rows (filter #(contains? % col) subsets)]
      (if (empty? rows)
        []
        (vec (mapcat (fn [row]
                       (let [covered (set row)
                             rest-u (apply disj universe covered)
                             rest-s (vec (remove #(some covered %) subsets))]
                         (search rest-u rest-s (conj solution row))))
                     rows))))))

(defn solve
  "Find all exact covers of `universe` drawn from `subsets`.
  Each solution is a vector of the chosen subsets."
  [universe subsets]
  (search (set universe) (mapv set subsets) []))
