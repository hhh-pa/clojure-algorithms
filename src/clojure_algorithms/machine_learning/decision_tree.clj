(ns clojure-algorithms.machine-learning.decision-tree
  "Basic regression decision tree over one-dimensional continuous data:
  maps a real input to a real output.

  Trees are immutable maps; train returns a new trained tree.")

(defn make-DecisionTree
  "Create an untrained tree with max depth and min leaf size."
  ([] (make-DecisionTree 5 5))
  ([depth] (make-DecisionTree depth 5))
  ([depth min-leaf-size]
   {:depth depth
    :decision-boundary 0
    :left nil
    :right nil
    :min-leaf-size min-leaf-size
    :prediction nil}))

(defn make-TestDecisionTree
  "Create the helper-test namespace object (all helpers are standalone)."
  []
  {})

(defn- mean-of
  [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- one-dimensional?
  [xs]
  (and (sequential? xs) (every? number? xs)))

(defn DecisionTree-mean-squared-error
  "Mean squared error of using prediction for labels."
  [_self labels prediction]
  (when-not (one-dimensional? labels)
    (println "Error: Input labels must be one dimensional"))
  (mean-of (map #(let [d (- (double %) (double prediction))] (* d d))
                (flatten labels))))

(defn TestDecisionTree-helper-mean-squared-error-test
  "Reference mean-squared-error implementation used for cross-checking."
  [labels prediction]
  (let [labels (vec (flatten labels))]
    (/ (double (reduce + (map #(let [d (- (double %) (double prediction))]
                                 (* d d))
                              labels)))
       (count labels))))

(defn DecisionTree-train
  "Train tree on 1-D x with labels y; returns the trained tree.

  Throws :ValueError when x is not one-dimensional, when x and y differ in
  length, or when y is not one-dimensional (checked in that order)."
  [self x y]
  (when-not (one-dimensional? x)
    (throw (ex-info "Input data set must be one-dimensional"
                    {:type :ValueError})))
  (when (not= (count x) (count y))
    (throw (ex-info "x and y have different lengths" {:type :ValueError})))
  (when-not (one-dimensional? y)
    (throw (ex-info "Data set labels must be one-dimensional"
                    {:type :ValueError})))
  (let [x (vec x)
        y (vec y)
        min-leaf (:min-leaf-size self)]
    (cond (< (count x) (* 2 min-leaf))
          (assoc self :prediction (mean-of y))

          (= (:depth self) 1)
          (assoc self :prediction (mean-of y))

          :else
          (let [min-error (* 2 (DecisionTree-mean-squared-error
                                self x (mean-of y)))
                ;; NOTE: like the upstream code, split errors are measured
                ;; on the x values against the y means (not on y itself).
                [best-split _]
                (reduce (fn [[best best-err] i]
                          (if (or (< (count (subvec x 0 i)) min-leaf)
                                  (< (count (subvec x i)) min-leaf))
                            [best best-err]
                            (let [error (+ (DecisionTree-mean-squared-error
                                            self (subvec x 0 i)
                                            (mean-of (subvec y 0 i)))
                                           (DecisionTree-mean-squared-error
                                            self (subvec x i)
                                            (mean-of (subvec y i))))]
                              (if (< error best-err) [i error] [best best-err]))))
                        [0 min-error]
                        (range (count x)))]
            (if (not= best-split 0)
              (let [make-child #(make-DecisionTree (dec (:depth self)) min-leaf)]
                (assoc self
                       :decision-boundary (nth x best-split)
                       :left (DecisionTree-train (make-child)
                                                 (subvec x 0 best-split)
                                                 (subvec y 0 best-split))
                       :right (DecisionTree-train (make-child)
                                                  (subvec x best-split)
                                                  (subvec y best-split))))
              (assoc self :prediction (mean-of y)))))))

(defn DecisionTree-predict
  "Predict the label of x by descending to a leaf.
  Throws :ValueError when the tree is not trained yet."
  [self x]
  (cond (some? (:prediction self)) (:prediction self)
        (and (some? (:left self)) (some? (:right self)))
        (if (>= x (:decision-boundary self))
          (DecisionTree-predict (:right self) x)
          (DecisionTree-predict (:left self) x))
        :else (throw (ex-info "Decision tree not yet trained"
                              {:type :ValueError}))))

(defn main
  "Demo: fit sin(x) on [-1, 1) and report the error on 10 random probes."
  []
  (let [x (vec (take-while #(< % 1.0) (iterate #(+ % 0.005) -1.0)))
        y (mapv #(Math/sin %) x)
        tree (DecisionTree-train (make-DecisionTree 10 10) x y)
        test-cases (vec (repeatedly 10 #(- (* (rand) 2) 1)))
        predictions (mapv #(DecisionTree-predict tree %) test-cases)
        avg-error (mean-of (map #(let [d (- %1 %2)] (* d d))
                                predictions test-cases))]
    (println (str "Test values: " test-cases))
    (println (str "Predictions: " predictions))
    (println (str "Average error: " avg-error))))
