(ns clojure-algorithms.fuzzy-logic.fuzzy-operations)

;; Note: plotting (FuzzySet.plot, matplotlib/numpy) is intentionally not
;; ported; all set algebra and membership logic below is pure.

(defrecord FuzzySet [name left-boundary peak right-boundary]
  Object
  (toString [this]
    (str name ": [" left-boundary ", " peak ", " right-boundary "]")))

(defn make-fuzzy-set
  "Create a triangular fuzzy set."
  [name left-boundary peak right-boundary]
  (->FuzzySet name left-boundary peak right-boundary))

(defn membership
  "Membership degree of x in the fuzzy set (triangular shape)."
  [fuzzy-set x]
  (cond
    (or (<= x (:left-boundary fuzzy-set)) (>= x (:right-boundary fuzzy-set))) 0.0
    (<= x (:peak fuzzy-set))
    (/ (- x (:left-boundary fuzzy-set)) (- (:peak fuzzy-set) (:left-boundary fuzzy-set)))
    (< x (:right-boundary fuzzy-set))
    (/ (- (:right-boundary fuzzy-set) x) (- (:right-boundary fuzzy-set) (:peak fuzzy-set)))
    :else (throw (ex-info (str "Invalid value " x " for fuzzy set " fuzzy-set) {}))))

(defn fuzzy-union
  "Union of two fuzzy sets."
  [a b]
  (->FuzzySet (str (:name a) " U " (:name b))
              (min (:left-boundary a) (:left-boundary b))
              (/ (+ (:peak a) (:peak b)) 2)
              (max (:right-boundary a) (:right-boundary b))))

(defn fuzzy-intersection
  "Intersection of two fuzzy sets."
  [a b]
  (->FuzzySet (str (:name a) " ∩ " (:name b))
              (max (:left-boundary a) (:left-boundary b))
              (min (:right-boundary a) (:right-boundary b))
              (/ (+ (:peak a) (:peak b)) 2)))

(defn fuzzy-complement
  "Complement (negation) of a fuzzy set."
  [fuzzy-set]
  (->FuzzySet (str "¬" (:name fuzzy-set))
              (- 1 (:right-boundary fuzzy-set))
              (- 1 (:left-boundary fuzzy-set))
              (- 1 (:peak fuzzy-set))))
