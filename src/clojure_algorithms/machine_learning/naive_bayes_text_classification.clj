(ns clojure-algorithms.machine-learning.naive-bayes-text-classification
  "Naive Bayes text classification using a multinomial event model.\n\nThe implementation in this module is intentionally educational and keeps the\nlogic explicit: token counting, prior probabilities, and posterior scoring in\nlog-space.\n\nReferences:\n- https://en.wikipedia.org/wiki/Naive_Bayes_classifier\n- https://scikit-learn.org/stable/modules/naive_bayes.html"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare build-toy-dataset make-NaiveBayesTextClassifier NaiveBayesTextClassifier-tokenize NaiveBayesTextClassifier-fit NaiveBayesTextClassifier-predict-proba NaiveBayesTextClassifier-predict)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-get-in py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-div py-min py-min-by py-max py-max-by py-sorted py-set py-dict-update py-counter py-float-str py-quote py-repr py-str py-log py-re-flags py-re-compile py-re-findall)

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

(defn- py-get-in [c ks]
  (if (and (instance? clojure.lang.IRecord c) (> (count ks) 1))
    (let [r (py-dunder :getitem c (vec ks))]
      (if (identical? r py-no-dunder)
        (reduce py-get c ks)
        r))
    (reduce py-get c ks)))

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

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

(defn- py-sorted [c]
  (cond (string? c) (vec (sort (map str (seq c))))
        (map? c) (vec (sort (keys c)))
        :else (try (vec (sort c))
                   (catch ClassCastException e
                     (throw (ex-info "not orderable" {:type :TypeError}))))))

(defn- py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-dict-update [d x extra]
  (let [d (if (map? d) d (throw (ex-info "update on non-dict" {:type :AttributeError})))]
    (merge d (cond (nil? x) {}
                   (map? x) x
                   (sequential? x) (into {} (map (fn [p] [(nth p 0) (nth p 1)]) x))
                   :else (throw (ex-info "cannot convert to dict" {:type :TypeError})))
           extra)))

(defn- py-counter [x]
  (cond (nil? x) {}
        (map? x) (into {} x)
        (string? x) (frequencies (map str (seq x)))
        :else (frequencies (py-iter x))))

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

(defn- py-log
  ([x] (if (and (number? (py-num-bool x)) (pos? (double x))) (Math/log (double x))
           (throw (ex-info "math domain error" {:type :ValueError}))))
  ([x b] (let [b (double b)]
           (when (or (neg? b) (zero? b)) (throw (ex-info "math domain error" {:type :ValueError})))
           (when (== b 1.0) (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
           (/ (py-log x) (Math/log b)))))

(defn- py-re-flags [f]
  (let [f (long (if (nil? f) 0 f))]
    (bit-or (if (pos? (bit-and f 2)) java.util.regex.Pattern/CASE_INSENSITIVE 0)
            (if (pos? (bit-and f 8)) java.util.regex.Pattern/MULTILINE 0)
            (if (pos? (bit-and f 16)) java.util.regex.Pattern/DOTALL 0)
            (if (pos? (bit-and f 64)) java.util.regex.Pattern/COMMENTS 0))))

(defn- py-re-compile [pat flags]
  (if (instance? java.util.regex.Pattern pat) pat
      (try (java.util.regex.Pattern/compile (str pat) (py-re-flags flags))
           (catch java.util.regex.PatternSyntaxException e
             (throw (ex-info (str e) {:type :ValueError}))))))

(defn- py-re-findall [pat s flags]
  (let [rx (py-re-compile pat flags)
        ^java.util.regex.Matcher m (re-matcher rx (str s))]
    (loop [out []]
      (if (.find m)
        (let [ng (.groupCount m)]
          (recur (conj out (cond (zero? ng) (.group m)
                                 (= ng 1) (.group m 1)
                                 :else (vec (map #(.group m (int %)) (range 1 (inc ng))))))))
        out))))

(defrecord NaiveBayesTextClassifier [alpha classes- vocabulary- class-document-counts- class-token-counts- class-total-tokens- class-log-prior- is-fitted-])

(defn make-NaiveBayesTextClassifier "Constructor for NaiveBayesTextClassifier. Multinomial Naive Bayes classifier for short text documents.\n\nArgs:\n    alpha: Additive (Laplace) smoothing parameter. Must be greater than 0.\n\n>>> NaiveBayesTextClassifier(alpha=0)\nTraceback (most recent call last):\n...\nValueError: alpha must be greater than 0."
  ([] (make-NaiveBayesTextClassifier 1.0))
  ([alpha] (let [self (atom {})] (try (when (py-truthy (py-le alpha 0)) (throw (ex-info (py-str "alpha must be greater than 0.") {:type :ValueError}))) (swap! self (fn [__c] (py-assoc-in __c [:alpha] alpha))) (swap! self (fn [__c] (py-assoc-in __c [:classes-] []))) (swap! self (fn [__c] (py-assoc-in __c [:vocabulary-] (py-set [])))) (swap! self (fn [__c] (py-assoc-in __c [:class-document-counts-] (py-counter [])))) (swap! self (fn [__c] (py-assoc-in __c [:class-token-counts-] {}))) (swap! self (fn [__c] (py-assoc-in __c [:class-total-tokens-] (py-counter [])))) (swap! self (fn [__c] (py-assoc-in __c [:class-log-prior-] {}))) (swap! self (fn [__c] (py-assoc-in __c [:is-fitted-] false))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) (map->NaiveBayesTextClassifier @self))))

(defn NaiveBayesTextClassifier-tokenize "Split text into lowercase alphanumeric tokens.\n\n>>> NaiveBayesTextClassifier._tokenize(\"Hello, NLP world!\")\n['hello', 'nlp', 'world']"
  [text] (py-re-findall "[a-z0-9']+" (clojure.string/lower-case @text) 0))

(defn NaiveBayesTextClassifier-fit "Fit the classifier from labeled training texts.\n\n>>> model = NaiveBayesTextClassifier()\n>>> model.fit([\"cheap meds\", \"project meeting\"], [\"spam\", \"ham\"])\n>>> sorted(model.classes_)\n['ham', 'spam']\n\n>>> model.fit([\"only one text\"], [\"ham\", \"spam\"])\nTraceback (most recent call last):\n...\nValueError: texts and labels must have the same length.\n\n>>> model.fit([], [])\nTraceback (most recent call last):\n...\nValueError: training data must not be empty."
  [self texts labels] (let [label (atom nil) text (atom nil) tokens (atom nil) total-documents (atom nil)] (when (py-truthy (not (py-truthy texts))) (throw (ex-info (py-str "training data must not be empty.") {:type :ValueError}))) (when (py-truthy (not (py-eq (count texts) (count labels)))) (throw (ex-info (py-str "texts and labels must have the same length.") {:type :ValueError}))) (swap! self (fn [__c] (py-assoc-in __c [:classes-] (py-sorted (py-set labels))))) nil nil (swap! self (fn [__c] (py-assoc-in __c [:class-token-counts-] {}))) nil nil (doseq [[__i1 __i2] (py-iter (map vector texts labels))] (reset! text __i1) (reset! label __i2) (when (py-truthy (let [__b3 (not (py-truthy (string? @text)))] (if (py-truthy __b3) __b3 (not (py-truthy (string? @label)))))) (throw (ex-info (py-str "texts and labels must contain strings only.") {:type :TypeError}))) (reset! tokens (NaiveBayesTextClassifier-tokenize self @text)) (swap! self (fn [__c] (py-assoc-in __c [:class-document-counts- @label] (py-add (py-get-in __c [:class-document-counts- @label]) 1)))) (py-dict-update (py-get (:class-token-counts- @self) @label) @tokens {}) (swap! self (fn [__c] (py-assoc-in __c [:class-total-tokens- @label] (py-add (py-get-in __c [:class-total-tokens- @label]) (count @tokens))))) (py-dict-update (:vocabulary- @self) @tokens {})) (reset! total-documents (count texts)) (swap! self (fn [__c] (py-assoc-in __c [:class-log-prior-] (into {} (for [label (py-iter (:classes- @self))] (list [label (py-log (py-div (py-get (:class-document-counts- @self) label) @total-documents))])))))) (swap! self (fn [__c] (py-assoc-in __c [:is-fitted-] true))) nil))

(defn NaiveBayesTextClassifier-predict-proba "Return posterior probabilities for every class.\n\n>>> train_texts, train_labels = build_toy_dataset()\n>>> model = NaiveBayesTextClassifier()\n>>> model.fit(train_texts, train_labels)\n>>> probs = model.predict_proba(\"cheap meds available now\")\n>>> round(sum(probs.values()), 6)\n1.0\n>>> probs['spam'] > probs['ham']\nTrue\n\nAn empty input text has no tokens, so predictions fall back to class priors.\n>>> empty_probs = model.predict_proba(\"\")\n>>> round(empty_probs['spam'], 3), round(empty_probs['ham'], 3)\n(0.5, 0.5)\n\n>>> NaiveBayesTextClassifier().predict_proba(\"hello\")\nTraceback (most recent call last):\n...\nValueError: model has not been fitted yet."
  [self text] (let [count-v (atom nil) denominator (atom nil) exp-scores (atom nil) label (atom nil) log-posteriors (atom nil) log-prob (atom nil) max-log (atom nil) normalizer (atom nil) token (atom nil) token-counts (atom nil) tokens (atom nil) vocabulary-size (atom nil)] (try (when (py-truthy (not (py-truthy (:is-fitted- @self)))) (throw (ex-info (py-str "model has not been fitted yet.") {:type :ValueError}))) (when (py-truthy (not (py-truthy (string? text)))) (throw (ex-info (py-str "text must be a string.") {:type :TypeError}))) (reset! tokens (NaiveBayesTextClassifier-tokenize self text)) (reset! vocabulary-size (count (:vocabulary- @self))) (reset! log-posteriors {}) (doseq [__i1 (py-iter (:classes- @self))] (reset! label __i1) (reset! log-prob (py-get (:class-log-prior- @self) @label)) (reset! token-counts (py-get (:class-token-counts- @self) @label)) (reset! denominator (py-add (py-get (:class-total-tokens- @self) @label) (py-mul (:alpha @self) @vocabulary-size))) (doseq [__i2 (py-iter @tokens)] (reset! token __i2) (reset! count-v (py-get @token-counts @token)) (swap! log-prob py-add (py-log (py-div (py-add @count-v (:alpha @self)) @denominator)))) (swap! log-posteriors (fn [__c] (py-assoc-in __c [@label] @log-prob)))) (reset! max-log (py-max (vec (vals @log-posteriors)))) (reset! exp-scores (into {} (for [[label score] (py-iter (vec @log-posteriors))] (list [label (Math/exp (py-sub score @max-log))])))) (reset! normalizer (reduce +' 0 (vec (vals @exp-scores)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (into {} (for [[label score] (py-iter (vec @exp-scores))] (list [label (py-div score @normalizer)])))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn NaiveBayesTextClassifier-predict "Predict the most likely class label for a text.\n\n>>> train_texts, train_labels = build_toy_dataset()\n>>> model = NaiveBayesTextClassifier(alpha=1.0)\n>>> model.fit(train_texts, train_labels)\n>>> model.predict(\"free cheap meds\")\n'spam'\n>>> model.predict(\"project meeting schedule\")\n'ham'"
  [self text] (let [probabilities (atom nil)] (try (reset! probabilities (NaiveBayesTextClassifier-predict-proba self text)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-max-by (fn [label] (py-get @probabilities label)) @probabilities)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn build-toy-dataset "Build a tiny text dataset for examples and quick local testing.\n\n>>> texts, labels = build_toy_dataset()\n>>> len(texts), len(labels)\n(6, 6)\n>>> sorted(set(labels))\n['ham', 'spam']"
  [] (let [labels (atom nil) texts (atom nil)] (try (reset! texts ["buy cheap meds now" "cheap meds available online" "win cash prizes now" "project meeting schedule attached" "let us discuss the project timeline" "team meeting moved to monday"]) (reset! labels ["spam" "spam" "spam" "ham" "ham" "ham"]) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@texts @labels]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
