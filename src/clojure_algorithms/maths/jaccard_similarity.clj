(ns clojure-algorithms.maths.jaccard-similarity
  "The Jaccard similarity coefficient is a commonly used indicator of the\nsimilarity between two sets. Let U be a set and A and B be subsets of U,\nthen the Jaccard index/similarity is defined to be the ratio of the number\nof elements of their intersection and the number of elements of their union.\n\nInspired from Wikipedia and\nthe book Mining of Massive Datasets [MMDS 2nd Edition, Chapter 3]\n\nhttps://en.wikipedia.org/wiki/Jaccard_index\nhttps://mmds.org\n\nJaccard similarity is widely used with MinHashing."
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare jaccard-similarity)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-div py-list py-float-str py-quote py-repr py-str)

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

(defn- py-list [x]
  (cond (nil? x) []
        (instance? clojure.lang.Atom x) (let [v (vec @x)] (reset! x []) v)
        (string? x) (mapv str (seq x))
        (map? x) (vec (keys x))
        (sequential? x) (vec x)
        (set? x) (vec x)
        (instance? clojure.lang.IRecord x) (vec (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

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

(defn jaccard-similarity "Finds the jaccard similarity between two sets.\nEssentially, its intersection over union.\n\nThe alternative way to calculate this is to take union as sum of the\nnumber of items in the two sets. This will lead to jaccard similarity\nof a set with itself be 1/2 instead of 1. [MMDS 2nd Edition, Page 77]\n\nParameters:\n    :set_a (set,list,tuple): A non-empty set/list\n    :set_b (set,list,tuple): A non-empty set/list\n    :alternativeUnion (boolean): If True, use sum of number of\n    items as union\n\nOutput:\n    (float) The jaccard similarity between the two sets.\n\nExamples:\n>>> set_a = {'a', 'b', 'c', 'd', 'e'}\n>>> set_b = {'c', 'd', 'e', 'f', 'h', 'i'}\n>>> jaccard_similarity(set_a, set_b)\n0.375\n>>> jaccard_similarity(set_a, set_a)\n1.0\n>>> jaccard_similarity(set_a, set_a, True)\n0.5\n>>> set_a = ['a', 'b', 'c', 'd', 'e']\n>>> set_b = ('c', 'd', 'e', 'f', 'h', 'i')\n>>> jaccard_similarity(set_a, set_b)\n0.375\n>>> set_a = ('c', 'd', 'e', 'f', 'h', 'i')\n>>> set_b = ['a', 'b', 'c', 'd', 'e']\n>>> jaccard_similarity(set_a, set_b)\n0.375\n>>> set_a = ('c', 'd', 'e', 'f', 'h', 'i')\n>>> set_b = ['a', 'b', 'c', 'd']\n>>> jaccard_similarity(set_a, set_b, True)\n0.2\n>>> set_a = {'a', 'b'}\n>>> set_b = ['c', 'd']\n>>> jaccard_similarity(set_a, set_b)\nTraceback (most recent call last):\n    ...\nValueError: Set a and b must either both be sets or be either a list or a tuple."
  ([set-a set-b] (jaccard-similarity set-a set-b false))
  ([set-a set-b alternative-union] (let [set-a (atom set-a)] (let [intersection (atom nil) intersection-length (atom nil) union (atom nil) union-length (atom nil)] (try (if (py-truthy (let [__b1 (set? @set-a)] (if (py-truthy __b1) (set? set-b) __b1))) (do (reset! intersection-length (count (clojure.set/intersection (set @set-a) (set set-b)))) (if (py-truthy alternative-union) (do (reset! union-length (py-add (count @set-a) (count set-b)))) (do (reset! union-length (count (clojure.set/union (set @set-a) (set set-b)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div @intersection-length @union-length)}))) (do (when (py-truthy (let [__b3 (let [__b2 (sequential? @set-a)] (if (py-truthy __b2) __b2 (sequential? @set-a)))] (if (py-truthy __b3) (or (sequential? set-b) (sequential? set-b)) __b3))) (reset! intersection (vec (for [element (py-iter @set-a) :when (py-truthy (py-in element set-b))] element))) (if (py-truthy alternative-union) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (count @intersection) (py-add (count @set-a) (count set-b)))}))) (do (reset! union (py-add (py-list @set-a) (vec (for [element (py-iter set-b) :when (py-truthy (not (py-in element @set-a)))] element)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-div (count @intersection) (count @union))}))))))) (throw (ex-info (py-str "Set a and b must either both be sets or be either a list or a tuple.") {:type :ValueError})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))))
