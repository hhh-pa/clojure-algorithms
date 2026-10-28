(ns clojure-algorithms.fractals.sierpinski-carpet)

(defn filled?
  "True when cell (row, col) belongs to the carpet. A cell is a hole
  when some pair of matching base-3 digits of row and col equals (1, 1)."
  [row col]
  (when (or (< row 0) (< col 0))
    (throw (ex-info (str "row and col must be non-negative, got (" row ", " col ")") {})))
  (loop [r row c col]
    (cond
      (and (= (mod r 3) 1) (= (mod c 3) 1)) false
      (and (zero? r) (zero? c)) true
      :else (recur (quot r 3) (quot c 3)))))

(defn generate-carpet
  "Sierpinski carpet of depth as a vector of strings. Depth 0 is a
  single filled cell; each level triples the side length."
  ([depth] (generate-carpet depth "#" " "))
  ([depth filled hole]
   (when (< depth 0)
     (throw (ex-info (str "depth must be non-negative, got " depth) {})))
   (let [size (long (Math/pow 3 depth))]
     (mapv (fn [row]
             (apply str (map (fn [col] (if (filled? row col) filled hole))
                             (range size))))
           (range size)))))

(defn count-filled-cells
  "Number of filled cells at depth: 8^depth."
  [depth]
  (when (< depth 0)
    (throw (ex-info (str "depth must be non-negative, got " depth) {})))
  (long (Math/pow 8 depth)))
