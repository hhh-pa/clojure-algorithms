(ns clojure-algorithms.machine-learning.linear-discriminant-analysis
  "Linear Discriminant Analysis over Gaussian class distributions.

  Covers dataset generation (seeded Gaussian sampling), estimation of class
  means / probabilities / variance, discriminant-based prediction and
  accuracy scoring, plus an interactive demo (main).

  Author (upstream): @EverLookNeverSee"
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Mersenne Twister (MT19937) matching CPython's `random` module, so the
;; seeded Gaussian stream is identical to the upstream doctests.
;; ---------------------------------------------------------------------------

(def ^:const ^:private mt-n 624)
(def ^:const ^:private mt-m 397)
(def ^:private mt-matrix-a (unchecked-int 0x9908b0df))
(def ^:private mt-upper-mask (unchecked-int 0x80000000))
(def ^:private mt-lower-mask (unchecked-int 0x7fffffff))

(defn- u32
  "View x as an unsigned 32-bit value (long). Needed because
  unsigned-bit-shift-right would otherwise shift the sign-extended 64-bit
  representation of negative ints."
  [x]
  (bit-and (long x) 0xFFFFFFFF))

(defn- mt-state
  "Fresh MT19937 state array seeded like CPython random.seed(1): init by the
  single-word key [1]. Layout: int-array of 625, last slot is the index."
  []
  (let [mt (int-array 625)]
    (aset mt 0 19650218)
    (dotimes [i 623]
      (let [prev (aget mt i)]
        (aset mt (inc i)
              (unchecked-int
               (unchecked-add
                (unchecked-multiply
                 1812433253
                 (unchecked-int (bit-xor prev (unsigned-bit-shift-right (u32 prev) 30))))
                (inc i))))))
    ;; init_by_array with key [1]
    (loop [i 1 j 0 k (max mt-n 1)]
      (when (pos? k)
        (let [prev (aget mt (dec i))]
          (aset mt i
                (unchecked-int
                 (unchecked-add
                  (unchecked-add
                   (bit-xor (aget mt i)
                            (unchecked-multiply
                             1664525
                             (unchecked-int
                              (bit-xor prev (unsigned-bit-shift-right (u32 prev) 30)))))
                   1)
                  j))))
        (let [i (inc i)
              i (if (>= i mt-n) (do (aset mt 0 (aget mt (dec mt-n))) 1) i)
              j (if (>= (inc j) 1) 0 (inc j))]
          (recur i j (dec k)))))
    (loop [i 2 k (dec mt-n)]
      (when (pos? k)
        (let [prev (aget mt (dec i))]
          (aset mt i
                (unchecked-int
                 (unchecked-subtract
                  (bit-xor (aget mt i)
                           (unchecked-multiply
                            1566083941
                            (unchecked-int
                             (bit-xor prev
                                      (unsigned-bit-shift-right (u32 prev) 30)))))
                  i))))
        (let [i (inc i)
              i (if (>= i mt-n) (do (aset mt 0 (aget mt (dec mt-n))) 1) i)]
          (recur i (dec k)))))
    (aset mt 0 mt-upper-mask)
    (aset mt mt-n mt-n)
    mt))

(defn- mt-twist!
  [mt]
  (dotimes [kk (- mt-n mt-m)]
    (let [y (unchecked-int (bit-or (bit-and (aget mt kk) mt-upper-mask)
                                   (bit-and (aget mt (inc kk)) mt-lower-mask)))]
      (aset mt kk (unchecked-int
                   (bit-xor (bit-xor (aget mt (+ kk mt-m))
                                     (unsigned-bit-shift-right (u32 y) 1))
                            (if (zero? (bit-and y 1)) 0 mt-matrix-a))))))
  (dotimes [kk (dec mt-m)]
    (let [i (+ kk (- mt-n mt-m))
          y (unchecked-int (bit-or (bit-and (aget mt i) mt-upper-mask)
                                   (bit-and (aget mt (inc i)) mt-lower-mask)))]
      (aset mt i (unchecked-int
                  (bit-xor (bit-xor (aget mt (+ i (- mt-m mt-n)))
                                    (unsigned-bit-shift-right (u32 y) 1))
                           (if (zero? (bit-and y 1)) 0 mt-matrix-a))))))
  (let [y (unchecked-int (bit-or (bit-and (aget mt (dec mt-n)) mt-upper-mask)
                                 (bit-and (aget mt 0) mt-lower-mask)))]
    (aset mt (dec mt-n)
          (unchecked-int
           (bit-xor (bit-xor (aget mt (dec mt-m))
                             (unsigned-bit-shift-right (u32 y) 1))
                    (if (zero? (bit-and y 1)) 0 mt-matrix-a))))))

(defn- mt-genrand!
  "Next unsigned-32 output (as int bits); twists when exhausted."
  [mt]
  (when (>= (aget mt mt-n) mt-n)
    (mt-twist! mt)
    (aset mt mt-n 0))
  (let [idx (aget mt mt-n)
        _ (aset mt mt-n (inc idx))
        y (aget mt idx)
        y (unchecked-int (bit-xor y (unsigned-bit-shift-right (u32 y) 11)))
        y (unchecked-int (bit-xor y (bit-and (unchecked-multiply y 128)
                                             (unchecked-int 0x9d2c5680))))
        y (unchecked-int (bit-xor y (bit-and (unchecked-multiply y 32768)
                                             (unchecked-int 0xefc60000))))
        y (unchecked-int (bit-xor y (unsigned-bit-shift-right (u32 y) 18)))]
    y))

(defn- mt-random!
  "Next double in [0, 1), like CPython random() (53-bit resolution)."
  [mt]
  (let [a (unsigned-bit-shift-right (u32 (mt-genrand! mt)) 5)
        b (unsigned-bit-shift-right (u32 (mt-genrand! mt)) 6)]
    (* (+ (* (double a) 67108864.0) (double b))
       (/ 1.0 9007199254740992.0))))

(defn- gauss-samples
  "n Box-Muller Gaussian samples with mean/std-dev from a seed-1 stream,
  matching CPython's random.gauss exactly."
  [mean std-dev n]
  (let [mt (mt-state)
        cached (atom nil)]
    (mapv (fn [_]
            (let [z (if-let [c @cached]
                      (do (reset! cached nil) c)
                      (let [x2pi (* (mt-random! mt) 6.283185307179586)
                            g2rad (Math/sqrt (* -2.0 (Math/log (- 1.0 (mt-random! mt)))))]
                        (reset! cached (* (Math/sin x2pi) g2rad))
                        (* (Math/cos x2pi) g2rad)))]
              (+ mean (* z std-dev))))
          (range n))))

;; ---------------------------------------------------------------------------
;; LDA
;; ---------------------------------------------------------------------------

(defn gaussian-distribution
  "n Gaussian samples with mean and std-dev, from a deterministically seeded
  stream (identical values to the upstream implementation on every call)."
  [mean std-dev instance-count]
  (gauss-samples (double mean) (double std-dev) instance-count))

(defn y-generator
  "Class label per instance: instance-count[k] copies of k.

  >>> (y-generator 2 [5 10])
  [0 0 0 0 0 1 1 1 1 1 1 1 1 1 1]"
  [class-count instance-count]
  (vec (mapcat #(repeat (nth instance-count %) %) (range class-count))))

(defn calculate-mean
  "Mean of the instance-count items."
  [instance-count items]
  (/ (double (reduce + items)) instance-count))

(defn calculate-probabilities
  "Class probability: instance-count / total-count."
  [instance-count total-count]
  (/ (double instance-count) total-count))

(defn calculate-variance
  "Pooled variance over per-class item lists with their means."
  [items means total-count]
  (let [squared (mapcat (fn [group mu]
                          (map #(let [d (- (double %) (double mu))] (* d d))
                               group))
                        items means)]
    (* (/ 1.0 (- total-count (count means))) (reduce + squared))))

(defn predict-y-values
  "Discriminant prediction per item: argmax over classes of
  x*(mean/variance) - mean^2/(2*variance) + log(probability)."
  [x-items means variance probabilities]
  (vec (mapcat
        (fn [group]
          (map (fn [x]
                 (let [scores (mapv (fn [mu p]
                                      (+ (- (* (double x)
                                               (/ (double mu) variance))
                                            (/ (* (double mu) (double mu))
                                               (* 2 variance)))
                                         (Math/log (double p))))
                                    means probabilities)]
                   ;; first maximal index wins, like list.index(max(...))
                   (first (keep-indexed
                           (fn [i s] (when (= s (apply max scores)) i))
                           scores))))
               group))
        x-items)))

(defn accuracy
  "Percentage of matching labels."
  [actual-y predicted-y]
  (let [correct (count (filter true? (map = actual-y predicted-y)))]
    (* (/ (double correct) (count actual-y)) 100.0)))

(def ^:private input-parsers
  {:int [(fn [s]
           (try (Integer/parseInt s)
                (catch NumberFormatException _
                  (throw (ex-info (str "invalid int: " s) {:type :ValueError})))))
         "int"]
   :float [(fn [s]
             (try (Double/parseDouble s)
                  (catch NumberFormatException _
                    (throw (ex-info (str "invalid float: " s)
                                    {:type :ValueError})))))
           "float"]})

(defn valid-input
  "Read a line (prompting with input-msg) and parse it with input-type
  (:int or :float), retrying until parsing succeeds and condition holds.
  An empty line falls back to default. Returns the parsed value.

  NOTE: on unparseable input the upstream code references an unbound
  variable (raising NameError); this port prints the offending input and
  retries instead."
  ([input-type input-msg err-msg] (valid-input input-type input-msg err-msg
                                               (fn [_] true) nil))
  ([input-type input-msg err-msg condition]
   (valid-input input-type input-msg err-msg condition nil))
  ([input-type input-msg err-msg condition default]
   (let [[parse type-name] (get input-parsers input-type [input-type "value"])]
     (loop []
       (print input-msg)
       (flush)
       (let [line (read-line)]
         (when (nil? line)
           (throw (ex-info "end of input" {:type :EOFError})))
         (let [raw (let [s (str/trim line)] (if (empty? s) default s))
               parsed (try (parse raw)
                           (catch clojure.lang.ExceptionInfo e
                             (if (= :ValueError (:type (ex-data e)))
                               ::invalid
                               (throw e))))]
           (cond (= parsed ::invalid)
                 (do (println (str raw ": Incorrect input type, expected '"
                                   type-name "'"))
                     (recur))

                 (condition parsed) parsed

                 :else (do (println (str parsed ": " err-msg))
                           (recur)))))))))

(defn main
  "Interactive LDA demo: read classes, generate data, report means,
  probabilities, variance and accuracy. Loops until the user quits with q.

  NOTE: the upstream screen-clear shell-out is omitted."
  []
  (loop []
    (println (str (apply str (repeat 12 "*")) " Linear Discriminant Analysis "
                  (apply str (repeat 12 "*"))))
    (println (apply str (repeat 50 "*")) "\n")
    (println "First of all we should specify the number of classes that")
    (println "we want to generate as training dataset")
    (let [n-classes (valid-input :int "Enter the number of classes (Data Groupings): "
                                 "Number of classes should be positive!"
                                 #(> % 0))]
      (println (apply str (repeat 100 "-")))
      (let [std-dev (valid-input :float (str "Enter the value of standard deviation"
                                             "(Default value is 1.0 for all classes): ")
                                 "Standard deviation should not be negative!"
                                 #(>= % 0) "1.0")]
        (println (apply str (repeat 100 "-")))
        (let [counts (mapv (fn [i]
                             (valid-input :int
                                          (str "Enter The number of instances for class_"
                                               (inc i) ": ")
                                          "Number of instances should be positive!"
                                          #(> % 0)))
                           (range n-classes))]
          (println (apply str (repeat 100 "-")))
          (let [user-means (mapv (fn [a]
                                   (valid-input :float
                                                (str "Enter the value of mean for class_"
                                                     (inc a) ": ")
                                                "This is an invalid value."))
                                 (range n-classes))]
            (println (apply str (repeat 100 "-")))
            (println "Standard deviation: " std-dev)
            (doseq [[i c] (map-indexed vector counts)]
              (println (str "Number of instances in class_" (inc i) " is: " c)))
            (println (apply str (repeat 100 "-")))
            (doseq [[i u] (map-indexed vector user-means)]
              (println (str "Mean of class_" (inc i) " is: " u)))
            (println (apply str (repeat 100 "-")))
            (let [x (mapv #(gaussian-distribution (nth user-means %) std-dev
                                                  (nth counts %))
                          (range n-classes))]
              (println "Generated Normal Distribution: \n" x)
              (println (apply str (repeat 100 "-")))
              (let [y (y-generator n-classes counts)]
                (println "Generated Corresponding Ys: \n" y)
                (println (apply str (repeat 100 "-")))
                (let [actual-means (mapv #(calculate-mean (nth counts %) (nth x %))
                                         (range n-classes))]
                  (doseq [[i a] (map-indexed vector actual-means)]
                    (println (str "Actual(Real) mean of class_" (inc i) " is: " a)))
                  (println (apply str (repeat 100 "-")))
                  (let [probabilities (mapv #(calculate-probabilities
                                              (nth counts %) (reduce + counts))
                                            (range n-classes))]
                    (doseq [[i p] (map-indexed vector probabilities)]
                      (println (str "Probability of class_" (inc i) " is: " p)))
                    (println (apply str (repeat 100 "-")))
                    (let [variance (calculate-variance x actual-means
                                                       (reduce + counts))]
                      (println "Variance: " variance)
                      (println (apply str (repeat 100 "-")))
                      (let [pre-indexes (predict-y-values x actual-means variance
                                                          probabilities)]
                        (println (apply str (repeat 100 "-")))
                        (println (str "Accuracy: " (accuracy y pre-indexes)))
                        (println (apply str (repeat 100 "-")))
                        (println (str (apply str (repeat 47 "+")) " DONE "
                                      (apply str (repeat 47 "+"))))
                        (print "Press any key to restart or 'q' for quit: ")
                        (flush)
                        (if (= "q" (str/lower-case
                                    (str/trim (or (read-line) "q"))))
                          (println "\n" (str (apply str (repeat 46 "-"))
                                             "GoodBye!"
                                             (apply str (repeat 46 "-"))) "\n")
                          (recur))))))))))))))
