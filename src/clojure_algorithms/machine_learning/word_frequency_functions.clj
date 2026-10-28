(ns clojure-algorithms.machine-learning.word-frequency-functions
  "Port of machine_learning/word_frequency_functions.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare term-frequency document-frequency inverse-document-frequency tf-idf)

(defn term-frequency "Return the number of times a term occurs within\na given document.\n@params: term, the term to search a document for, and document,\n        the document to search within\n@returns: an integer representing the number of times a term is\n        found within the document\n\n@examples:\n>>> term_frequency(\"to\", \"To be, or not to be\")\n2"
  [& _args]
  ;; stub: original requires method .maketrans; pure-Clojure port not applicable
  (throw (ex-info "requires method .maketrans" {:type :UnsupportedOperation})))

(defn document-frequency "Calculate the number of documents in a corpus that contain a\ngiven term\n@params : term, the term to search each document for, and corpus, a collection of\n         documents. Each document should be separated by a newline.\n@returns : the number of documents in the corpus that contain the term you are\n           searching for and the number of documents in the corpus\n@examples :\n>>> document_frequency(\"first\", \"This is the first document in the corpus.\\nThIsis the second document in the corpus.\\nTHIS is the third document in the corpus.\")\n(1, 3)"
  [& _args]
  ;; stub: original requires method .maketrans; pure-Clojure port not applicable
  (throw (ex-info "requires method .maketrans" {:type :UnsupportedOperation})))

(defn inverse-document-frequency "Return an integer denoting the importance\nof a word. This measure of importance is\ncalculated by log10(N/df), where N is the\nnumber of documents and df is\nthe Document Frequency.\n@params : df, the Document Frequency, N,\nthe number of documents in the corpus and\nsmoothing, if True return the idf-smooth\n@returns : log10(N/df) or 1+log10(N/1+df)\n@examples :\n>>> inverse_document_frequency(3, 0)\nTraceback (most recent call last):\n ...\nValueError: log10(0) is undefined.\n>>> inverse_document_frequency(1, 3)\n0.477\n>>> inverse_document_frequency(0, 3)\nTraceback (most recent call last):\n ...\nZeroDivisionError: df must be > 0\n>>> inverse_document_frequency(0, 3,True)\n1.477"
  ([df n] (inverse-document-frequency df n false))
  ([df n smoothing] (try (when (py-truthy smoothing) (when (py-truthy (py-eq n 0)) (throw (ex-info (py-str "log10(0) is undefined.") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-round (py-add 1 (Math/log10 (py-div n (py-add 1 df)))) 3)}))) (when (py-truthy (py-eq df 0)) (throw (ex-info (py-str "df must be > 0") {:type :ZeroDivisionError}))) (when (py-truthy (py-eq n 0)) (throw (ex-info (py-str "log10(0) is undefined.") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-round (Math/log10 (py-div n df)) 3)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn tf-idf "Combine the term frequency\nand inverse document frequency functions to\ncalculate the originality of a term. This\n'originality' is calculated by multiplying\nthe term frequency and the inverse document\nfrequency : tf-idf = TF * IDF\n@params : tf, the term frequency, and idf, the inverse document\nfrequency\n@examples :\n>>> tf_idf(2, 0.477)\n0.954"
  [tf idf] (py-round (py-mul tf idf) 3))

;; NOTE: term_frequency stubbed (method .maketrans)
;; NOTE: document_frequency stubbed (method .maketrans)
