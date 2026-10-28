(ns clojure-algorithms.machine-learning.naive-bayes-text-classification
  "Multinomial Naive Bayes text classification.

  The model is an immutable map; fit returns a new fitted model. Scoring is
  done in log-space with Laplace smoothing.

  References:
  https://en.wikipedia.org/wiki/Naive_Bayes_classifier
  https://scikit-learn.org/stable/modules/naive_bayes.html"
  (:require [clojure.string :as str]))

(defn make-NaiveBayesTextClassifier
  "Create an unfitted classifier with Laplace smoothing alpha.
  Throws :ValueError unless alpha is positive."
  ([] (make-NaiveBayesTextClassifier 1.0))
  ([alpha]
   (when (<= alpha 0)
     (throw (ex-info "alpha must be greater than 0." {:type :ValueError})))
   {:alpha alpha
    :classes- []
    :vocabulary- #{}
    :class-document-counts- {}
    :class-token-counts- {}
    :class-total-tokens- {}
    :class-log-prior- {}
    :is-fitted- false}))

(defn NaiveBayesTextClassifier-tokenize
  "Split text into lowercase alphanumeric tokens.

  >>> (NaiveBayesTextClassifier-tokenize \"Hello, NLP world!\")
  [\"hello\" \"nlp\" \"world\"]"
  [text]
  (vec (re-seq #"[a-z0-9']+" (str/lower-case text))))

(defn NaiveBayesTextClassifier-fit
  "Fit the classifier on texts with labels; returns the fitted model.

  Throws :ValueError on empty or length-mismatched data, :TypeError on
  non-string texts or labels."
  [model texts labels]
  (when (empty? texts)
    (throw (ex-info "training data must not be empty." {:type :ValueError})))
  (when (not= (count texts) (count labels))
    (throw (ex-info "texts and labels must have the same length."
                    {:type :ValueError})))
  (doseq [[text label] (map vector texts labels)]
    (when (or (not (string? text)) (not (string? label)))
      (throw (ex-info "texts and labels must contain strings only."
                      {:type :TypeError}))))
  (let [classes (vec (sort (set labels)))
        per-label (group-by second (map vector texts labels))
        doc-counts (into {} (map (fn [[label pairs]] [label (count pairs)])
                                 per-label))
        token-counts
        (into {} (map (fn [[label pairs]]
                        [label (frequencies
                                (mapcat #(NaiveBayesTextClassifier-tokenize
                                          (first %))
                                        pairs))])
                      per-label))
        total-tokens (into {} (map (fn [[label counts]]
                                     [label (reduce + 0 (vals counts))])
                                   token-counts))
        total-docs (count texts)]
    (assoc model
           :classes- classes
           :vocabulary- (set (mapcat #(keys (get token-counts %)) classes))
           :class-document-counts- doc-counts
           :class-token-counts- token-counts
           :class-total-tokens- total-tokens
           :class-log-prior-
           (into {} (map (fn [label]
                           [label (Math/log (/ (double (get doc-counts label))
                                               total-docs))])
                         classes))
           :is-fitted- true)))

(defn NaiveBayesTextClassifier-predict-proba
  "Posterior probability per class for text (softmax over log-posteriors).

  Throws :ValueError when unfitted, :TypeError when text is not a string."
  [model text]
  (when-not (:is-fitted- model)
    (throw (ex-info "model has not been fitted yet." {:type :ValueError})))
  (when-not (string? text)
    (throw (ex-info "text must be a string." {:type :TypeError})))
  (let [tokens (NaiveBayesTextClassifier-tokenize text)
        vocab-size (count (:vocabulary- model))
        log-posteriors
        (into {}
              (map (fn [label]
                     (let [counts (get (:class-token-counts- model) label)
                           denom (+ (get (:class-total-tokens- model) label)
                                    (* (:alpha model) vocab-size))]
                       [label (+ (get (:class-log-prior- model) label)
                                 (reduce + 0
                                         (map #(Math/log
                                                (/ (+ (double (get counts % 0))
                                                      (:alpha model))
                                                   denom))
                                              tokens)))]))
                   (:classes- model)))
        max-log (apply max (vals log-posteriors))
        scores (into {} (map (fn [[label score]]
                               [label (Math/exp (- score max-log))])
                             log-posteriors))
        normalizer (reduce + (vals scores))]
    (into {} (map (fn [[label score]] [label (/ score normalizer)]) scores))))

(defn NaiveBayesTextClassifier-predict
  "Most likely class label for text (first maximal class wins ties)."
  [model text]
  (let [probs (NaiveBayesTextClassifier-predict-proba model text)]
    (reduce (fn [best label]
              (if (> (get probs label) (get probs best)) label best))
            (:classes- model))))

(defn build-toy-dataset
  "Tiny labeled corpus: [texts labels] with 3 spam and 3 ham messages."
  []
  [["buy cheap meds now"
    "cheap meds available online"
    "win cash prizes now"
    "project meeting schedule attached"
    "let us discuss the project timeline"
    "team meeting moved to monday"]
   ["spam" "spam" "spam" "ham" "ham" "ham"]])
