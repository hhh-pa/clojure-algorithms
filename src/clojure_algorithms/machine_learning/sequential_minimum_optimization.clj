(ns clojure-algorithms.machine-learning.sequential-minimum-optimization
  "Sequential Minimal Optimization (SMO) for Support Vector Machines.

  The Kernel (linear/polynomial/RBF) and the pure SmoSVM helpers (K-matrix
  lookup, error evaluation, prediction, alpha bounds, norm, support queries)
  are plain arithmetic and fully ported. The fit loop is driven by
  bidirectional Python generators (coroutine send protocol) with no
  stdlib-only equivalent, and the test/demo entry points need sklearn,
  pandas, network access or matplotlib, so those are honestly stubbed.

  References:
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/smo-book.pdf
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tr-98-14.pdf")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(def CANCER-DATASET-URL
  "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")

(declare Kernel-check SmoSVM-is-unbound SmoSVM-length)

;; ---------------------------------------------------------------------------
;; Kernel
;; ---------------------------------------------------------------------------

(defn- kernel-fn-for
  [kernel-name]
  (case kernel-name
    "linear" :linear
    "poly" :poly
    "rbf" :rbf
    (throw (ex-info (str "unknown kernel: " kernel-name) {:type :KeyError}))))

(defn make-Kernel
  "Kernel of the given name (\"linear\", \"poly\" or \"rbf\").
  Throws :KeyError for unknown names, :ValueError for a negative RBF gamma."
  ([kernel-name] (make-Kernel kernel-name 1.0 0.0 1.0))
  ([kernel-name degree] (make-Kernel kernel-name degree 0.0 1.0))
  ([kernel-name degree coef0] (make-Kernel kernel-name degree coef0 1.0))
  ([kernel-name degree coef0 gamma]
   (let [kernel {:degree (double degree) :coef0 (double coef0)
                 :gamma (double gamma) :-kernel-name kernel-name
                 :-kernel (kernel-fn-for kernel-name)}]
     (Kernel-check kernel)
     kernel)))

(defn Kernel-polynomial
  "(gamma * <v1,v2> + coef0)^degree."
  [self v1 v2]
  (Math/pow (+ (* (:gamma self) (reduce + (map * v1 v2))) (:coef0 self))
            (:degree self)))

(defn Kernel-linear
  "<v1,v2> + coef0."
  [self v1 v2]
  (+ (reduce + (map * v1 v2)) (:coef0 self)))

(defn Kernel-rbf
  "exp(-gamma * ||v1 - v2||^2)."
  [self v1 v2]
  (Math/exp (* -1.0 (:gamma self)
               (reduce + (map #(let [d (- (double %1) (double %2))] (* d d))
                              v1 v2)))))

(defn Kernel-check
  "Validate the kernel: RBF needs a non-negative gamma. Returns nil."
  [self]
  (when (and (= :rbf (:-kernel self)) (< (:gamma self) 0))
    (throw (ex-info "gamma value must be non-negative" {:type :ValueError})))
  nil)

(defn Kernel-get-kernel
  "Resolve a kernel name to its keyword. Throws :KeyError when unknown."
  [_self kernel-name]
  (kernel-fn-for kernel-name))

(defn Kernel-call
  "Apply the kernel to two vectors."
  [self v1 v2]
  (case (:-kernel self)
    :linear (Kernel-linear self v1 v2)
    :poly (Kernel-polynomial self v1 v2)
    :rbf (Kernel-rbf self v1 v2)))

(defn Kernel-repr
  "The kernel name."
  [self]
  (:-kernel-name self))

;; ---------------------------------------------------------------------------
;; SmoSVM
;; ---------------------------------------------------------------------------

(defn- normalize-with
  [samples col-min col-max]
  (mapv (fn [row]
          (mapv (fn [x lo hi] (/ (- (double x) lo) (- hi lo)))
                row col-min col-max))
        samples))

(defn make-SmoSVM
  "SmoSVM over train rows (first column tags, rest sample features).
  Precomputes normalization, the kernel matrix and the error vector.
  NOTE: :choose-alpha is nil because the generator-driven chooser has no
  stdlib-only equivalent (SmoSVM-fit is stubbed for the same reason)."
  ([train kernel-func] (make-SmoSVM train kernel-func nil 0.4 0.0 0.001 true))
  ([train kernel-func alpha-list cost b tolerance auto-norm]
   (let [tags (mapv first train)
         raw (mapv #(vec (rest %)) train)
         col-min (when auto-norm
                   (mapv #(apply min %) (apply mapv vector raw)))
         col-max (when auto-norm
                   (mapv #(apply max %) (apply mapv vector raw)))
         samples (if auto-norm (normalize-with raw col-min col-max) raw)
         n (count train)
         self {:init false :auto-norm auto-norm
               :c (double cost) :b (double b)
               :tol (if (> tolerance 0.0001) (double tolerance) 0.001)
               :tags tags :samples samples
               :alphas (vec (or alpha-list (repeat n 0.0)))
               :Kernel kernel-func :eps 0.001
               :all-samples (vec (range n))
               :error (vec (repeat n 0.0))
               :unbound [] :min col-min :max col-max
               :choose-alpha nil}
         k-matrix (vec (for [i (range n)]
                         (vec (for [j (range n)]
                                (Kernel-call kernel-func (nth samples i)
                                             (nth samples j))))))]
     (assoc self :-K-matrix k-matrix))))

(defn SmoSVM-fit
  "STUB: the fit loop drives bidirectional Python generators (send protocol)."
  [& _args]
  (unsupported! "generator send protocol"))

(defn SmoSVM-k
  "Kernel value: K-matrix lookup for training index pairs, direct kernel
  evaluation when i2 is a sample vector."
  [self i1 i2]
  (if (sequential? i2)
    (Kernel-call (:Kernel self) (nth (:samples self) i1) i2)
    (get-in self [:-K-matrix i1 i2])))

(defn SmoSVM-e
  "Error for a sample: cached value when unbound, else g(xi) - yi."
  [self index]
  (if (SmoSVM-is-unbound self index)
    (nth (:error self) index)
    (let [col (mapv #(nth % index) (:-K-matrix self))
          gx (+ (reduce + (map * (:alphas self) (:tags self) col))
                (:b self))]
      (- gx (nth (:tags self) index)))))

(defn SmoSVM-calculate-k-matrix
  "Full kernel matrix over the training samples."
  [self]
  (let [n (count (:all-samples self))]
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (Kernel-call (:Kernel self)
                               (nth (:samples self) i)
                               (nth (:samples self) j))))))))

(defn- smo-predict-one
  [self sample]
  (+ (reduce + (map (fn [i1]
                      (* (nth (:alphas self) i1)
                         (nth (:tags self) i1)
                         (SmoSVM-k self i1 sample)))
                    (:all-samples self)))
     (:b self)))

(defn SmoSVM-predict
  "Classify (:classify true) or score each test sample.
  Throws :ValueError when test features outnumber train features."
  ([self test-samples] (SmoSVM-predict self test-samples true))
  ([self test-samples classify]
   (when (> (count (first test-samples)) (count (first (:samples self))))
     (throw (ex-info (str "Test samples' feature length does not equal to "
                          "that of train samples")
                     {:type :ValueError})))
   (let [test-samples (if (:auto-norm self)
                        (normalize-with test-samples (:min self) (:max self))
                        test-samples)]
     (mapv (fn [sample]
             (let [result (smo-predict-one self sample)]
               (if classify (if (> result 0) 1 -1) result)))
           test-samples))))

(defn SmoSVM-check-obey-kkt
  "True when alpha[index] violates the KKT conditions."
  [self index]
  (let [r (* (SmoSVM-e self index) (nth (:tags self) index))
        c (:c self)
        a (nth (:alphas self) index)
        tol (:tol self)]
    (boolean (or (and (< r (- tol)) (< a c))
                 (and (> r tol) (> a 0.0))))))

(defn SmoSVM-choose-alphas
  "STUB: bidirectional generator over violating alpha pairs."
  [& _args]
  (unsupported! "generator send protocol"))

(defn SmoSVM-choose-a1
  "STUB: bidirectional generator over first-alpha candidates."
  [& _args]
  (unsupported! "generator send protocol"))

(defn SmoSVM-choose-a2
  "STUB: bidirectional generator over second-alpha candidates."
  [& _args]
  (unsupported! "generator send protocol"))

(defn SmoSVM-get-new-alpha
  "Constrained two-alpha update; [nil nil] when no step applies."
  [self i1 i2 a1 a2 e1 e2 y1 y2]
  (if (= i1 i2)
    [nil nil]
    (let [c (:c self)
          s (* y1 y2)
          [l h] (if (= s -1)
                  [(max 0.0 (- a2 a1)) (min c (- (+ c a2) a1))]
                  [(max 0.0 (- (+ a2 a1) c)) (min c (+ a2 a1))])]
      (if (= l h)
        [nil nil]
        (let [k11 (SmoSVM-k self i1 i1)
              k22 (SmoSVM-k self i2 i2)
              k12 (SmoSVM-k self i1 i2)
              eta (- (+ k11 k22) (* 2.0 k12))
              a2-new (if (> eta 0.0)
                       (let [unc (+ a2 (/ (* y2 (- e1 e2)) eta))]
                         (cond (>= unc h) h (<= unc l) l :else unc))
                       (let [b (:b self)
                             l1 (+ a1 (* s (- a2 l)))
                             h1 (+ a1 (* s (- a2 h)))
                             f1 (- (- (* y1 (+ e1 b)) (* a1 k11))
                                   (* (* s a2) k12))
                             f2 (- (- (* y2 (+ e2 b)) (* a2 k22))
                                   (* (* s a1) k12))
                             obj (fn [a1v a2v]
                                   (+ (* a1v f1) (* a2v f2)
                                      (* (* 0.5 a1v a1v) k11)
                                      (* (* 0.5 a2v a2v) k22)
                                      (* (* (* s a2v) a1v) k12)))
                             ol (obj l1 l)
                             oh (obj h1 h)
                             eps (:eps self)]
                         (cond (< ol (- oh eps)) l (> ol (+ oh eps)) h :else a2)))
              a1-new (+ a1 (* s (- a2 a2-new)))
              [a1-new a2-new]
              (cond (< a1-new 0) [0 (+ a2-new (* s a1-new))]
                    (> a1-new c) [c (+ a2-new (* s (- a1-new c)))]
                    :else [a1-new a2-new])]
          [a1-new a2-new])))))

(defn SmoSVM-norm
  "Min-max normalize rows: stored bounds apply once fitted, otherwise the
  data's own bounds are used (purely, without persisting them)."
  [self data]
  (let [[col-min col-max] (if (:init self)
                            [(mapv #(apply min %) (apply mapv vector data))
                             (mapv #(apply max %) (apply mapv vector data))]
                            [(:min self) (:max self)])]
    (normalize-with data col-min col-max)))

(defn SmoSVM-is-unbound
  [self index]
  (boolean (< 0.0 (nth (:alphas self) index) (:c self))))

(defn SmoSVM-is-support
  [self index]
  (boolean (> (nth (:alphas self) index) 0)))

(defn SmoSVM-unbound
  [self]
  (:unbound self))

(defn SmoSVM-support
  [self]
  (vec (filter #(SmoSVM-is-support self %) (range (SmoSVM-length self)))))

(defn SmoSVM-length
  [self]
  (count (:samples self)))

(defn count-time
  "Wrap f to print its wall-clock cost. Returns f's value."
  [f]
  (fn [& args]
    (let [start (System/nanoTime)
          result (apply f args)
          elapsed (/ (- (System/nanoTime) start) 1e9)]
      (println (str "SMO algorithm cost " elapsed " seconds"))
      result)))

(defn test-cancer-data
  "STUB: downloads the cancer dataset (needs urllib/pandas)."
  [& _args]
  (unsupported! "urllib,pd"))

(defn test-demonstration
  "STUB: needs matplotlib."
  [& _args]
  (unsupported! "plt"))

(defn test-linear-kernel
  "STUB: needs sklearn (make_blobs, StandardScaler)."
  [& _args]
  (unsupported! "make_blobs"))

(defn test-rbf-kernel
  "STUB: needs sklearn (make_circles, StandardScaler)."
  [& _args]
  (unsupported! "make_circles"))

(defn plot-partition-boundary
  "STUB: needs matplotlib (contour of the partition boundary)."
  [& _args]
  (unsupported! "plt"))
