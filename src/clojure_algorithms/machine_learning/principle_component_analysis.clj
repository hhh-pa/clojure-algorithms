(ns clojure-algorithms.machine-learning.principle-component-analysis
  "Principal Component Analysis (PCA) is a dimensionality reduction technique\nused in machine learning. It transforms high-dimensional data into a lower-dimensional\nrepresentation while retaining as much variance as possible.\n\nThis implementation follows best practices, including:\n- Standardizing the dataset.\n- Computing principal components using Singular Value Decomposition (SVD).\n- Returning transformed data and explained variance ratio."
  (:require [clojure.string :as str]))

(declare collect-dataset apply-pca main)

(declare py-slice py-float-str py-quote py-repr py-str)

(defn- py-slice [c lo hi step]
  (when (map? c) (throw (ex-info "unhashable slice" {:type :TypeError})))
  (let [s (vec (seq c)) n (count s)
        step (if (nil? step) 1 step)]
    (when (zero? step) (throw (ex-info "slice step cannot be zero" {:type :ValueError})))
    (let [idxs (if (pos? step)
                 (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                        (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                        step)
                 (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                        (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                        step))
          res (mapv s idxs)]
      (if (string? c) (apply str res) res))))

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

(defn collect-dataset "Collects the dataset (Iris dataset) and returns feature matrix and target values.\n\n:return: Tuple containing feature matrix (X) and target labels (y)\n\nExample:\n>>> X, y = collect_dataset()\n>>> X.shape\n(150, 4)\n>>> y.shape\n(150,)"
  [& _args]
  ;; stub: original requires load_iris,np; pure-Clojure port not applicable
  (throw (ex-info "requires load_iris,np" {:type :UnsupportedOperation})))

(defn apply-pca "Applies Principal Component Analysis (PCA) to reduce dimensionality.\n\n:param data_x: Original dataset (features)\n:param n_components: Number of principal components to retain\n:return: Tuple containing transformed dataset and explained variance ratio\n\nExample:\n>>> X, _ = collect_dataset()\n>>> transformed_X, variance = apply_pca(X, 2)\n>>> transformed_X.shape\n(150, 2)\n>>> len(variance) == 2\nTrue"
  [& _args]
  ;; stub: original requires PCA,StandardScaler,np; pure-Clojure port not applicable
  (throw (ex-info "requires PCA,StandardScaler,np" {:type :UnsupportedOperation})))

(defn main "Driver function to execute PCA and display results."
  [] (let [-data-y (atom nil) data-x (atom nil) n-components (atom nil) transformed-data (atom nil) variance-ratio (atom nil)] (let [__d1 (vec (collect-dataset))] (reset! data-x (nth __d1 0)) (reset! -data-y (nth __d1 1))) (reset! n-components 2) (let [__d2 (vec (apply-pca @data-x @n-components))] (reset! transformed-data (nth __d2 0)) (reset! variance-ratio (nth __d2 1))) (println (clojure.string/join " " [(py-str "Transformed Dataset (First 5 rows):")])) (println (clojure.string/join " " [(py-str (py-slice @transformed-data nil 5 nil))])) (println (clojure.string/join " " [(py-str "\nExplained Variance Ratio:")])) (println (clojure.string/join " " [(py-str @variance-ratio)])) nil))

;; NOTE: collect_dataset stubbed (load_iris,np)
;; NOTE: apply_pca stubbed (PCA,StandardScaler,np)
