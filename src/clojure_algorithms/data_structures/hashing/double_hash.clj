(ns clojure-algorithms.data-structures.hashing.double-hash
  "Port of data_structures/hashing/double_hash.py.
  Hash table with double-hash collision resolution."
  (:require [clojure-algorithms.data-structures.hashing.number-theory.prime-numbers
             :refer [is-prime next-prime]]))

(defn make-double-hash
  "Double-hash table with SIZE-TABLE slots."
  ([size-table] (make-double-hash size-table 1 0.75))
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

(defn- hash-function-2 [{:keys [size-table]} value data]
  (let [r (mod value size-table)
        np (if (is-prime r) r (next-prime r))]
    (- np (mod data np))))

(defn- hash-double-function [ht key data increment]
  (mod (* increment (hash-function-2 ht key data)) (:size-table ht)))

(defn- collision-resolution [{:keys [values lim-charge] :as ht} key data]
  (loop [i 1 new-key (hash-function ht data)]
    (if (and (some? (nth values new-key)) (not= (nth values new-key) key))
      (let [new-key (when (>= (balanced-factor ht) lim-charge)
                      (hash-double-function ht key data i))]
        (if (nil? new-key) nil (recur (inc i) new-key)))
      new-key)))

(declare insert-data)

(defn- rehashing [{:keys [values size-table] :as ht}]
  (let [survivors (vec (filter some? values))
        new-size (next-prime size-table 2)
        ht (assoc ht :size-table new-size :keys {}
                  :values (vec (repeat new-size nil)))]
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
