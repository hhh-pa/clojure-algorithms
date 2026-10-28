(ns clojure-algorithms.data-structures.hashing.hash-table
  "Port of data_structures/hashing/hash_table.py.
  Linear-probing hash table over immutable vectors.
  State is {:size-table :values :lim-charge :charge-factor :keys}."
  (:require [clojure-algorithms.data-structures.hashing.number-theory.prime-numbers
             :refer [next-prime]]))

(defn make-hash-table
  "Hash table with SIZE-TABLE slots."
  ([size-table] (make-hash-table size-table 1 0.75))
  ([size-table charge-factor lim-charge]
   {:size-table size-table
    :values (vec (repeat size-table nil))
    :lim-charge lim-charge
    :charge-factor charge-factor
    :keys {}}))

(defn ht-keys
  "Key-to-data map."
  [ht]
  (:keys ht))

(defn balanced-factor
  "Fraction of filled slots over capacity."
  [{:keys [values size-table charge-factor]}]
  (/ (double (count (filter some? values))) (* size-table charge-factor)))

(defn hash-function
  "Hash KEY into a slot."
  [{:keys [size-table]} key]
  (mod key size-table))

(defn- set-value [ht key data]
  (-> ht
      (assoc-in [:values key] data)
      (assoc-in [:keys key] data)))

(defn- collision-resolution [{:keys [values] :as ht} key _data]
  (loop [new-key (hash-function ht (inc key))]
    (if (and (some? (nth values new-key)) (not= (nth values new-key) key))
      (if (some nil? values)
        (recur (hash-function ht (inc new-key)))
        nil)
      new-key)))

(declare insert-data)

(defn- rehashing [{:keys [values size-table] :as ht}]
  (let [survivors (vec (filter some? values))
        ht (assoc ht :size-table (next-prime size-table 2)
                  :keys {}
                  :values (vec (repeat (next-prime size-table 2) nil)))]
    (reduce insert-data ht survivors)))

(defn insert-data
  "Insert DATA; returns the new table."
  [{:keys [values] :as ht} data]
  (let [key (hash-function ht data)]
    (cond
      (nil? (nth values key)) (set-value ht key data)
      (= (nth values key) data) ht
      :else (let [resolved (collision-resolution ht key data)]
              (if (some? resolved)
                (set-value ht resolved data)
                (insert-data (rehashing ht) data))))))

(defn bulk-insert
  "Insert VALUES printing each step like the Python version.
  Returns the new table."
  [ht values]
  (reduce (fn [ht [i v]]
            (let [ht (insert-data ht v)]
              (println (str "step " i))
              (println (vec (range (count (:values ht)))))
              (println (:values ht))
              ht))
          ht (map-indexed (fn [i v] [(inc i) v]) values)))
