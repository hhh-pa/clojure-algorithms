(ns clojure-algorithms.machine-learning.automatic-differentiation
  "Reverse-mode automatic differentiation sketch.

  The computation-graph plumbing (operation types, operation nodes and the
  singleton gradient tracker with its graph wiring) is plain structural
  logic and fully ported. Variables wrap numpy ndarrays and every value
  computation (elementwise/broadcast arithmetic, matmul, power, gradient
  accumulation) needs ndarray semantics with no stdlib-only equivalent, so
  the value-carrying functions are honest documented stubs.

  Reference: https://en.wikipedia.org/wiki/Automatic_differentiation")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(def OpType
  "Supported operation types for gradient calculation."
  {:ADD 0 :SUB 1 :MUL 2 :DIV 3 :MATMUL 4 :POWER 5 :NOOP 6})

(def OpType-ADD 0)
(def OpType-SUB 1)
(def OpType-MUL 2)
(def OpType-DIV 3)
(def OpType-MATMUL 4)
(def OpType-POWER 5)
(def OpType-NOOP 6)

(defn make-OpType
  "Resolve an op-type id to its keyword (unknown values pass through)."
  [v]
  (get {0 :ADD 1 :SUB 2 :MUL 3 :DIV 4 :MATMUL 5 :POWER 6 :NOOP} v v))

(defn make-Variable
  "STUB: variables wrap numpy ndarrays (needs np.array)."
  [& _args]
  (unsupported! "np"))

(defn Variable-to-ndarray
  "STUB: returns the wrapped numpy array."
  [& _args]
  (unsupported! "np"))

(defn Variable-add
  "STUB: elementwise/broadcasting ndarray addition."
  [& _args]
  (unsupported! "np"))

(defn Variable-sub
  "STUB: elementwise/broadcasting ndarray subtraction."
  [& _args]
  (unsupported! "np"))

(defn Variable-mul
  "STUB: elementwise/broadcasting ndarray multiplication."
  [& _args]
  (unsupported! "np"))

(defn Variable-truediv
  "STUB: elementwise/broadcasting ndarray division."
  [& _args]
  (unsupported! "np"))

(defn Variable-matmul
  "STUB: ndarray matrix multiplication."
  [& _args]
  (unsupported! "np"))

(defn Variable-pow
  "STUB: ndarray power."
  [& _args]
  (unsupported! "np"))

(defn Variable-repr
  "Variable(<value>) rendering for a variable-like map."
  [variable]
  (let [variable (if (instance? clojure.lang.Atom variable) @variable variable)]
    (str "Variable(" (:value variable) ")")))

(defn Variable-add-param-to
  "Record an operation the variable feeds into. Returns nil."
  [variable operation]
  (swap! variable update :param-to (fnil conj []) operation)
  nil)

(defn Variable-add-result-of
  "Record the operation producing the variable. Returns nil."
  [variable operation]
  (swap! variable assoc :result-of operation)
  nil)

(defn make-Operation
  "Operation node of op-type with optional other-params."
  ([op-type] (make-Operation op-type nil))
  ([op-type other-params]
   {:op-type op-type :other-params (or other-params {})}))

(defn Operation-add-params
  "Return the operation with input param nodes attached."
  [operation params]
  (assoc operation :params params))

(defn Operation-add-output
  "Return the operation with its output variable attached."
  [operation output]
  (assoc operation :output output))

(defn Operation-eq
  "True when value is an op-type id equal to the operation's type."
  [operation value]
  (boolean (and (contains? (set (vals OpType)) value)
                (= (:op-type operation) value))))

(def GradientTracker-instance
  "The singleton tracker state (enabled flag)."
  (atom {:enabled false}))

(defn make-GradientTracker
  "Return the singleton tracker (fresh instances share state upstream too:
  constructing always resets enabled to false)."
  []
  (reset! GradientTracker-instance {:enabled false})
  GradientTracker-instance)

(defn GradientTracker-enter
  "Enable the tracker; returns it."
  [tracker]
  (swap! tracker assoc :enabled true)
  tracker)

(defn GradientTracker-exit
  "Disable the tracker; returns nil."
  [tracker _exc-type _exc _traceback]
  (swap! tracker assoc :enabled false)
  nil)

(defn GradientTracker-append
  "Wire an operation into the computation graph: every param records the
  operation, the output records it as its source, and the operation records
  its params and output. Params/output are variable atoms. Returns nil."
  ([tracker op-type params output]
   (GradientTracker-append tracker op-type params output nil))
  ([_tracker op-type params output other-params]
   (let [operation (-> (make-Operation op-type other-params)
                       (Operation-add-params (vec params))
                       (Operation-add-output output))]
     (doseq [param params]
       (Variable-add-param-to param operation))
     (Variable-add-result-of output operation)
     nil)))

(defn GradientTracker-gradient
  "STUB: reverse accumulation over ndarray partial derivatives."
  [& _args]
  (unsupported! "np"))

(defn GradientTracker-derivative
  "STUB: per-operation ndarray derivatives (needs transpose/broadcasting)."
  [& _args]
  (unsupported! "np"))
