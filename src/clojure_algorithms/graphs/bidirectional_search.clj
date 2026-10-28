(ns clojure-algorithms.graphs.bidirectional-search
  "Shortest path via simultaneous forward/backward BFS.")

(defn- expand-one
  "Expands one BFS frontier step; returns [queue parents intersection]."
  [graph queue parents opposite-parents]
  (if (empty? queue)
    [queue parents nil]
    (let [current (peek queue)
          queue (pop queue)]
      (loop [neighbours (get graph current []) queue queue parents parents]
        (if (empty? neighbours)
          [queue parents nil]
          (let [n (first neighbours)]
            (if (contains? parents n)
              (recur (rest neighbours) queue parents)
              (let [parents (assoc parents n current)
                    queue (conj queue n)]
                (if (contains? opposite-parents n)
                  [queue parents n]
                  (recur (rest neighbours) queue parents))))))))))

(defn- construct-path [current parents]
  (loop [c current path []]
    (if (nil? c) path (recur (get parents c) (conj path c)))))

(defn bidirectional-search
  "Returns the start->goal path, or nil when no path exists."
  [graph start goal]
  (cond
    (= start goal) [start]
    (or (not (contains? graph start)) (not (contains? graph goal))) nil
    :else
    (let [q (clojure.lang.PersistentQueue/EMPTY)]
      (loop [fq (conj q start) bq (conj q goal)
             fp {start nil} bp {goal nil}]
        (if (or (empty? fq) (empty? bq))
          nil
          (let [[fq fp hit] (expand-one graph fq fp bp)]
            (if hit
              (let [fwd (vec (reverse (construct-path hit fp)))
                    bwd (construct-path (get bp hit) bp)]
                (vec (concat fwd bwd)))
              (let [[bq bp hit] (expand-one graph bq bp fp)]
                (if hit
                  (let [fwd (vec (reverse (construct-path hit fp)))
                        bwd (construct-path (get bp hit) bp)]
                    (vec (concat fwd bwd)))
                  (recur fq bq fp bp))))))))))
