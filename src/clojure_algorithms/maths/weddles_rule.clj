(ns clojure-algorithms.maths.weddles-rule
  "Port of maths/weddles_rule.py")

(defn- parse-input-double
  "Parse s as a double, throwing :ValueError like Python's float()."
  [s]
  (if (nil? s)
    (throw (ex-info "unexpected end of input" {:type :ValueError}))
    (try
      (Double/parseDouble s)
      (catch NumberFormatException _
        (throw (ex-info (str "could not convert string to float: '" s "'")
                        {:type :ValueError}))))))

(defn get-inputs
  "Read the integrand expression, lower limit and upper limit from stdin.
  Returns [function-string lower-limit upper-limit]."
  []
  (print "Enter function with variable as x: ")
  (flush)
  (let [func (read-line)]
    (print "Enter lower limit: ")
    (flush)
    (let [lower-limit (read-line)]
      (print "Enter upper limit: ")
      (flush)
      (let [upper-limit (read-line)]
        [func (parse-input-double lower-limit) (parse-input-double upper-limit)]))))

;; A small safe parser for arithmetic in the variable x, standing in for
;; sympy: numbers, x, pi, E, + - * / // % **, parentheses and single
;; argument math functions, with Python operator precedence.

(def ^:private fn-table
  "Single-argument math functions available in expressions."
  {"sin" #(Math/sin ^double %)
   "cos" #(Math/cos ^double %)
   "tan" #(Math/tan ^double %)
   "asin" #(Math/asin ^double %)
   "acos" #(Math/acos ^double %)
   "atan" #(Math/atan ^double %)
   "exp" #(Math/exp ^double %)
   "log" #(Math/log ^double %)
   "log10" #(Math/log10 ^double %)
   "sqrt" #(Math/sqrt ^double %)
   "cbrt" #(Math/cbrt ^double %)
   "abs" #(Math/abs ^double %)
   "floor" #(Math/floor ^double %)
   "ceil" #(Math/ceil ^double %)})

(defn- parse-failure
  "Throw a :ValueError for an unparseable expression."
  [func-str]
  (throw (ex-info (str "could not parse expression: '" func-str "'")
                  {:type :ValueError})))

(declare parse-expr parse-factor)

(defn- tokenize
  "Split the expression into a vector of tokens."
  [func-str]
  (let [n (count func-str)]
    (loop [i 0
           tokens []]
      (if (>= i n)
        tokens
        (let [c (.charAt ^String func-str i)]
          (cond
            (Character/isWhitespace c)
            (recur (inc i) tokens)

            (or (Character/isDigit c)
                (and (= c \.) (< (inc i) n)
                     (Character/isDigit (.charAt ^String func-str (inc i)))))
            (let [m (re-matcher #"\G(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?"
                                func-str)]
              (.find m i)
              (recur (.end m) (conj tokens [:num (Double/parseDouble (.group m))])))

            (or (Character/isLetter c) (= c \_))
            (let [m (re-matcher #"\G[A-Za-z_][A-Za-z0-9_]*" func-str)]
              (.find m i)
              (let [word (.group m)]
                (recur (.end m)
                       (conj tokens (cond (= word "x") :x
                                          (= word "pi") [:const Math/PI]
                                          (= word "E") [:const Math/E]
                                          (contains? fn-table word) [:fn word]
                                          :else (parse-failure func-str))))))

            (and (= c \*) (< (inc i) n) (= (.charAt ^String func-str (inc i)) \*))
            (recur (+ i 2) (conj tokens :pow))

            (and (= c \/) (< (inc i) n) (= (.charAt ^String func-str (inc i)) \/))
            (recur (+ i 2) (conj tokens :floordiv))

            (= c \+) (recur (inc i) (conj tokens :+))
            (= c \-) (recur (inc i) (conj tokens :-))
            (= c \*) (recur (inc i) (conj tokens :*))
            (= c \/) (recur (inc i) (conj tokens :/))
            (= c \%) (recur (inc i) (conj tokens :%))
            (= c \() (recur (inc i) (conj tokens :lparen))
            (= c \)) (recur (inc i) (conj tokens :rparen))
            :else (parse-failure func-str)))))))

(defn- parse-primary
  "Parse a number, x, constant, call, or parenthesized expression.
  Returns [evaluator-fn next-index]."
  [func-str toks i]
  (let [t (nth toks i nil)]
    (cond
      (nil? t) (parse-failure func-str)
      (= t :x) [(fn [x] (double x)) (inc i)]
      (and (vector? t) (= (first t) :num)) [(constantly (second t)) (inc i)]
      (and (vector? t) (= (first t) :const)) [(constantly (second t)) (inc i)]
      (and (vector? t) (= (first t) :fn))
      (let [g (get fn-table (second t))]
        (when-not (= (nth toks (inc i) nil) :lparen)
          (parse-failure func-str))
        (let [[h j] (parse-expr func-str toks (+ i 2))]
          (when-not (= (nth toks j nil) :rparen)
            (parse-failure func-str))
          [(fn [x] (g (h x))) (inc j)]))
      (= t :lparen)
      (let [[h j] (parse-expr func-str toks (inc i))]
        (when-not (= (nth toks j nil) :rparen)
          (parse-failure func-str))
        [h (inc j)])
      :else (parse-failure func-str))))

(defn- parse-power
  "Parse primary ['**' factor], right associative."
  [func-str toks i]
  (let [[f j] (parse-primary func-str toks i)]
    (if (= (nth toks j nil) :pow)
      (let [[g k] (parse-factor func-str toks (inc j))]
        [(fn [x] (Math/pow (f x) (g x))) k])
      [f j])))

(defn- parse-factor
  "Parse unary +/- or a power."
  [func-str toks i]
  (let [t (nth toks i nil)]
    (cond (= t :-) (let [[f j] (parse-factor func-str toks (inc i))]
                     [(fn [x] (- (f x))) j])
          (= t :+) (parse-factor func-str toks (inc i))
          :else (parse-power func-str toks i))))

(defn- parse-term
  "Parse factor (('*' | '/' | '//' | '%') factor)*."
  [func-str toks i]
  (let [[f j] (parse-factor func-str toks i)]
    (loop [f f
           j j]
      (let [t (nth toks j nil)]
        (cond
          (= t :*) (let [[g k] (parse-factor func-str toks (inc j))]
                     (recur (fn [x] (* (f x) (g x))) k))
          (= t :/) (let [[g k] (parse-factor func-str toks (inc j))]
                     (recur (fn [x] (/ (f x) (g x))) k))
          (= t :floordiv) (let [[g k] (parse-factor func-str toks (inc j))]
                            (recur (fn [x] (Math/floor (/ (f x) (g x)))) k))
          (= t :%) (let [[g k] (parse-factor func-str toks (inc j))]
                     (recur (fn [x]
                              (let [a (f x)
                                    b (g x)]
                                (- a (* b (Math/floor (/ a b))))))
                            k))
          :else [f j])))))

(defn- parse-expr
  "Parse term (('+' | '-') term)*."
  [func-str toks i]
  (let [[f j] (parse-term func-str toks i)]
    (loop [f f
           j j]
      (let [t (nth toks j nil)]
        (cond
          (= t :+) (let [[g k] (parse-term func-str toks (inc j))]
                     (recur (fn [x] (+ (f x) (g x))) k))
          (= t :-) (let [[g k] (parse-term func-str toks (inc j))]
                     (recur (fn [x] (- (f x) (g x))) k))
          :else [f j])))))

(defn safe-function-eval
  "Build a callable evaluator for an arithmetic expression in x.
  Supports numbers, x, pi, E, + - * / // % **, parentheses and
  single-argument math functions. Throws :ValueError when the
  expression cannot be parsed."
  [func-str]
  (let [toks (tokenize func-str)
        [f j] (if (empty? toks)
                (parse-failure func-str)
                (parse-expr func-str toks 0))]
    (when-not (= j (count toks))
      (parse-failure func-str))
    f))

(defn compute-table
  "Evaluate func at acc * 6 + 1 evenly spaced points from lower-limit to
  upper-limit. Returns [table step-size]."
  [func lower-limit upper-limit acc]
  (let [n-points (inc (* acc 6))
        h (/ (- (double upper-limit) (double lower-limit)) (dec n-points))
        xs (mapv (fn [i] (+ (double lower-limit) (* i h))) (range n-points))]
    [(mapv (fn [x] (double (func x))) xs) h]))

(defn apply-weights
  "Apply Weddle's rule weights to the interior table values."
  [table]
  (loop [i 1
         add []]
    (if (>= i (dec (count table)))
      add
      (recur (inc i)
             (conj add (cond (and (even? i) (not (zero? (mod i 3))))
                             (nth table i)

                             (and (odd? i) (not (zero? (mod i 3))))
                             (* 5 (nth table i))

                             (zero? (mod i 6))
                             (* 2 (nth table i))

                             :else
                             (* 6 (nth table i))))))))

(defn compute-solution
  "Combine the weighted values, table endpoints and step size into the
  final Weddle's rule integral estimate."
  [add table step-size]
  (* 0.3 step-size (+ (reduce + 0.0 add) (first table) (last table))))
