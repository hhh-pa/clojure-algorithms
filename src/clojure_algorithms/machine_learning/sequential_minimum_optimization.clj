(ns clojure-algorithms.machine-learning.sequential-minimum-optimization
  "Sequential minimal optimization (SMO) for support vector machines (SVM)\n\nSequential minimal optimization (SMO) is an algorithm for solving the quadratic\nprogramming (QP) problem that arises during the training of SVMs. It was invented by\nJohn Platt in 1998.\n\nInput:\n    0: type: numpy.ndarray.\n    1: first column of ndarray must be tags of samples, must be 1 or -1.\n    2: rows of ndarray represent samples.\n\nUsage:\n    Command:\n        python3 sequential_minimum_optimization.py\n    Code:\n        from sequential_minimum_optimization import SmoSVM, Kernel\n\n        kernel = Kernel(kernel='poly', degree=3., coef0=1., gamma=0.5)\n        init_alphas = np.zeros(train.shape[0])\n        SVM = SmoSVM(train=train, alpha_list=init_alphas, kernel_func=kernel, cost=0.4,\n                     b=0.0, tolerance=0.001)\n        SVM.fit()\n        predict = SVM.predict(test_samples)\n\nReference:\n    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/smo-book.pdf\n    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tr-98-14.pdf"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare count-time test-cancer-data test-demonstration test-linear-kernel test-rbf-kernel plot-partition-boundary make-SmoSVM SmoSVM-fit SmoSVM-predict SmoSVM-check-obey-kkt SmoSVM-k SmoSVM-e SmoSVM-calculate-k-matrix SmoSVM-choose-alphas SmoSVM-choose-a1 SmoSVM-choose-a2 SmoSVM-get-new-alpha SmoSVM-norm SmoSVM-is-unbound SmoSVM-is-support SmoSVM-unbound SmoSVM-support SmoSVM-length make-Kernel Kernel-polynomial Kernel-linear Kernel-rbf Kernel-check Kernel-get-kernel Kernel-call Kernel-repr CANCER-DATASET-URL)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-add py-sub py-repeat-vec py-mul py-div py-pow py-neg py-min py-min-by py-max py-max-by py-float-str py-quote py-repr py-str)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

(defrecord SmoSVM [-init -auto-norm -c -b -tol tags samples alphas Kernel -eps -all-samples -K-matrix -error -unbound choose-alpha -min -max])

(defrecord Kernel [degree coef0 gamma -kernel-name -kernel])

(defn make-SmoSVM "Constructor for SmoSVM."
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn make-Kernel "Constructor for Kernel."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn SmoSVM-fit
  [& _args]
  ;; stub: original requires np,numpy-attr:copy; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:copy" {:type :UnsupportedOperation})))

(defn SmoSVM-predict
  [& _args]
  ;; stub: original requires np,numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape" {:type :UnsupportedOperation})))

(defn SmoSVM-check-obey-kkt
  [self index] (let [alphas (atom nil) c (atom nil) r (atom nil) tol (atom nil)] (try (reset! alphas (:alphas @self)) (reset! tol (:-tol @self)) (reset! r (py-mul (SmoSVM-e self index) (py-get (:tags @self) index))) (reset! c (:-c @self)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (let [__b3 (let [__b1 (py-lt @r (py-neg @tol))] (if (py-truthy __b1) (py-lt (py-get @alphas index) @c) __b1))] (if (py-truthy __b3) __b3 (let [__b2 (py-gt @r @tol)] (if (py-truthy __b2) (py-gt (py-get @alphas index) 0.0) __b2))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn SmoSVM-k
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn SmoSVM-e "Two cases:\n    1: Sample[index] is non-bound, fetch error from list: _error\n    2: sample[index] is bound, use predicted value minus true value: g(xi) - yi"
  [& _args]
  ;; stub: original requires np,numpy-attr:dot; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:dot" {:type :UnsupportedOperation})))

(defn SmoSVM-calculate-k-matrix
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn SmoSVM-predict
  [& _args]
  ;; stub: original requires np,numpy-attr:sum; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:sum" {:type :UnsupportedOperation})))

(defn SmoSVM-choose-alphas
  [self] (let [__ys (atom []) loci (atom nil)] (try (reset! loci (do (swap! __ys into (vec (SmoSVM-choose-a1 self))) nil)) (when (py-truthy (not (py-truthy @loci))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @loci})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn SmoSVM-choose-a1 "Choose first alpha\nSteps:\n    1: First loop over all samples\n    2: Second loop over all non-bound samples until no non-bound samples violate\n       the KKT condition.\n    3: Repeat these two processes until no samples violate the KKT condition\n       after the first loop."
  [self] (let [__ys (atom []) all-not-obey (atom nil) i1 (atom nil) not-obey (atom nil)] (try (try (while (py-truthy true) (reset! all-not-obey true) (println (clojure.string/join " " [(py-str "Scanning all samples!")])) (doseq [__i1 (py-iter (vec (for [i (py-iter (:-all-samples @self)) :when (py-truthy (SmoSVM-check-obey-kkt self i))] i)))] (reset! i1 __i1) (reset! all-not-obey false) (do (swap! __ys into (vec (SmoSVM-choose-a2 self @i1))) nil)) (println (clojure.string/join " " [(py-str "Scanning non-bound samples!")])) (try (while (py-truthy true) (reset! not-obey true) (doseq [__i3 (py-iter (vec (for [i (py-iter (:-all-samples @self)) :when (py-truthy (let [__b2 (SmoSVM-check-obey-kkt self i)] (if (py-truthy __b2) (SmoSVM-is-unbound self i) __b2)))] i)))] (reset! i1 __i3) (reset! not-obey false) (do (swap! __ys into (vec (SmoSVM-choose-a2 self @i1))) nil)) (when (py-truthy @not-obey) (println (clojure.string/join " " [(py-str "All non-bound samples satisfy the KKT condition!")])) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (when (py-truthy @all-not-obey) (println (clojure.string/join " " [(py-str "All samples satisfy the KKT condition!")])) (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn SmoSVM-choose-a2 "Choose the second alpha using a heuristic algorithm\nSteps:\n    1: Choose alpha2 that maximizes the step size (|E1 - E2|).\n    2: Start in a random point, loop over all non-bound samples till alpha1 and\n       alpha2 are optimized.\n    3: Start in a random point, loop over all samples till alpha1 and alpha2 are\n       optimized."
  [& _args]
  ;; stub: original requires np,numpy-attr:copy,numpy-attr:tolist; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:copy,numpy-attr:tolist" {:type :UnsupportedOperation})))

(defn SmoSVM-get-new-alpha
  [self i1 i2 a1 a2 e1 e2 y1 y2] (let [a1-new (atom nil) a2-new (atom nil) a2-new-unc (atom nil) b (atom nil) eta (atom nil) f1 (atom nil) f2 (atom nil) h (atom nil) h1 (atom nil) k (atom nil) k11 (atom nil) k12 (atom nil) k22 (atom nil) l (atom nil) l1 (atom nil) oh (atom nil) ol (atom nil) s (atom nil)] (try (reset! k (partial SmoSVM-k self)) (when (py-truthy (py-eq i1 i2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [nil nil]}))) (reset! s (py-mul y1 y2)) (if (py-truthy (py-eq @s (py-neg 1))) (do (let [__d1 (vec [(py-max [0.0 (py-sub a2 a1)]) (py-min [(:-c @self) (py-sub (py-add (:-c @self) a2) a1)])])] (reset! l (nth __d1 0)) (reset! h (nth __d1 1)))) (do (let [__d2 (vec [(py-max [0.0 (py-sub (py-add a2 a1) (:-c @self))]) (py-min [(:-c @self) (py-add a2 a1)])])] (reset! l (nth __d2 0)) (reset! h (nth __d2 1))))) (when (py-truthy (py-eq @l @h)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [nil nil]}))) (reset! k11 (@k i1 i1)) (reset! k22 (@k i2 i2)) (reset! k12 (@k i1 i2)) (if (py-truthy (py-gt (do (reset! eta (py-sub (py-add @k11 @k22) (py-mul 2.0 @k12))) @eta) 0.0)) (do (reset! a2-new-unc (py-add a2 (py-div (py-mul y2 (py-sub e1 e2)) @eta))) (if (py-truthy (py-ge @a2-new-unc @h)) (do (reset! a2-new @h)) (do (if (py-truthy (py-le @a2-new-unc @l)) (do (reset! a2-new @l)) (do (reset! a2-new @a2-new-unc)))))) (do (reset! b (:-b @self)) (reset! l1 (py-add a1 (py-mul @s (py-sub a2 @l)))) (reset! h1 (py-add a1 (py-mul @s (py-sub a2 @h)))) (reset! f1 (py-sub (py-sub (py-mul y1 (py-add e1 @b)) (py-mul a1 (@k i1 i1))) (py-mul (py-mul @s a2) (@k i1 i2)))) (reset! f2 (py-sub (py-sub (py-mul y2 (py-add e2 @b)) (py-mul a2 (@k i2 i2))) (py-mul (py-mul @s a1) (@k i1 i2)))) (reset! ol (py-add (py-add (py-add (py-add (py-mul @l1 @f1) (py-mul @l @f2)) (py-mul (py-mul (py-div 1 2) (py-pow @l1 2)) (@k i1 i1))) (py-mul (py-mul (py-div 1 2) (py-pow @l 2)) (@k i2 i2))) (py-mul (py-mul (py-mul @s @l) @l1) (@k i1 i2)))) (reset! oh (py-add (py-add (py-add (py-add (py-mul @h1 @f1) (py-mul @h @f2)) (py-mul (py-mul (py-div 1 2) (py-pow @h1 2)) (@k i1 i1))) (py-mul (py-mul (py-div 1 2) (py-pow @h 2)) (@k i2 i2))) (py-mul (py-mul (py-mul @s @h) @h1) (@k i1 i2)))) (if (py-truthy (py-lt @ol (py-sub @oh (:-eps @self)))) (do (reset! a2-new @l)) (do (if (py-truthy (py-gt @ol (py-add @oh (:-eps @self)))) (do (reset! a2-new @h)) (do (reset! a2-new a2))))))) (reset! a1-new (py-add a1 (py-mul @s (py-sub a2 @a2-new)))) (when (py-truthy (py-lt @a1-new 0)) (swap! a2-new py-add (py-mul @s @a1-new)) (reset! a1-new 0)) (when (py-truthy (py-gt @a1-new (:-c @self))) (swap! a2-new py-add (py-mul @s (py-sub @a1-new (:-c @self)))) (reset! a1-new (:-c @self))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@a1-new @a2-new]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn SmoSVM-norm
  [& _args]
  ;; stub: original requires np,numpy-attr:max,numpy-attr:min; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:max,numpy-attr:min" {:type :UnsupportedOperation})))

(defn SmoSVM-is-unbound
  [self index] (boolean (py-truthy (let [__c1 0.0 __c2 (py-get (:alphas @self) index) __c3 (:-c @self)] (and (py-lt __c1 __c2) (py-lt __c2 __c3))))))

(defn SmoSVM-is-support
  [self index] (boolean (py-truthy (py-gt (py-get (:alphas @self) index) 0))))

(defn SmoSVM-unbound
  [self] (:-unbound @self))

(defn SmoSVM-support
  [self] (vec (for [i (py-iter (range (SmoSVM-length self))) :when (py-truthy (SmoSVM-is-support self i))] i)))

(defn SmoSVM-length
  [& _args]
  ;; stub: original requires numpy-attr:shape; pure-Clojure port not applicable
  (throw (ex-info "requires numpy-attr:shape" {:type :UnsupportedOperation})))

(defn Kernel-polynomial
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Kernel-linear
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Kernel-rbf
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn Kernel-check
  [self] (when (py-truthy (let [__b1 (py-eq (:-kernel @self) (partial Kernel-rbf self))] (if (py-truthy __b1) (py-lt (:gamma @self) 0) __b1))) (throw (ex-info (py-str "gamma value must be non-negative") {:type :ValueError}))) nil)

(defn Kernel-get-kernel
  [self kernel-name] (let [maps (atom nil)] (try (reset! maps {"linear" (partial Kernel-linear self) "poly" (partial Kernel-polynomial self) "rbf" (partial Kernel-rbf self)}) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get @maps kernel-name)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn Kernel-call
  [& _args]
  ;; stub: original requires method ._kernel; pure-Clojure port not applicable
  (throw (ex-info "requires method ._kernel" {:type :UnsupportedOperation})))

(defn Kernel-repr
  [self] (:-kernel-name @self))

(defn count-time
  [& _args]
  ;; stub: original requires kwargs for func; pure-Clojure port not applicable
  (throw (ex-info "requires kwargs for func" {:type :UnsupportedOperation})))

(defn test-cancer-data
  [& _args]
  ;; stub: original requires np,numpy-attr:shape,numpy-attr:tolist,pd,urllib; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:shape,numpy-attr:tolist,pd,urllib" {:type :UnsupportedOperation})))

(defn test-demonstration
  [& _args]
  ;; stub: original requires plt; pure-Clojure port not applicable
  (throw (ex-info "requires plt" {:type :UnsupportedOperation})))

(defn test-linear-kernel
  [& _args]
  ;; stub: original requires StandardScaler,make_blobs,np,numpy-attr:reshape; pure-Clojure port not applicable
  (throw (ex-info "requires StandardScaler,make_blobs,np,numpy-attr:reshape" {:type :UnsupportedOperation})))

(defn test-rbf-kernel
  [& _args]
  ;; stub: original requires StandardScaler,make_circles,np,numpy-attr:reshape; pure-Clojure port not applicable
  (throw (ex-info "requires StandardScaler,make_circles,np,numpy-attr:reshape" {:type :UnsupportedOperation})))

(defn plot-partition-boundary "We cannot get the optimal w of our kernel SVM model, which is different from a\nlinear SVM.  For this reason, we generate randomly distributed points with high\ndensity, and predicted values of these points are calculated using our trained\nmodel. Then we could use this predicted values to draw contour map, and this contour\nmap represents the SVM's partition boundary."
  [& _args]
  ;; stub: original requires np,numpy-attr:T,numpy-attr:max,numpy-attr:min,numpy-attr:reshape,plt; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:T,numpy-attr:max,numpy-attr:min,numpy-attr:reshape,plt" {:type :UnsupportedOperation})))

(def CANCER-DATASET-URL "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")

(defmethod print-method Kernel [o w] (.write w (Kernel-repr (atom o))))

;; NOTE: SmoSVM __init__ stubbed (np,numpy-attr:shape)
;; NOTE: SmoSVM.fit stubbed (np,numpy-attr:copy)
;; NOTE: SmoSVM.predict stubbed (np,numpy-attr:shape)
;; NOTE: SmoSVM._k stubbed (np)
;; NOTE: SmoSVM._e stubbed (np,numpy-attr:dot)
;; NOTE: SmoSVM._calculate_k_matrix stubbed (np)
;; NOTE: SmoSVM._predict stubbed (np,numpy-attr:sum)
;; NOTE: SmoSVM._choose_a2 stubbed (np,numpy-attr:copy,numpy-attr:tolist)
;; NOTE: SmoSVM._norm stubbed (np,numpy-attr:max,numpy-attr:min)
;; NOTE: SmoSVM.length stubbed (numpy-attr:shape)
;; NOTE: Kernel __init__ stubbed (np)
;; NOTE: Kernel._polynomial stubbed (np)
;; NOTE: ... and 9 more
