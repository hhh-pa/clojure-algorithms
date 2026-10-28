(ns clojure-algorithms.data-structures.hashing.hash-table-with-linked-list
  "Port of data_structures/hashing/hash_table_with_linked_list.py.
  Hash table chaining colliding values in per-slot lists (vectors here).
  State is {:size-table :values :lim-charge :charge-factor :keys}."
  (:require [clojure-algorithms.data-structures.hashing.number-theory.prime-numbers
             :refer [next-prime]]))

(defn make-hash-table
  "Chained hash table with SIZE-TABLE slots."
  ([size-table] (make-hash-table size-table 1 0.75))
  ([size-table charge-factor lim-charge]
   {:size-table size-table
    :values (vec (repeat size-table nil))
    :lim-charge lim-charge
    :charge-factor charge-factor
    :keys {}}))

(defn ht-keys
  "Slot-to-chain map."
  [ht]
  (:keys ht))

(defn balanced-factor
  "Load metric from the Python version."
  [{:keys [values size-table charge-factor]}]
  (* (/ (double (reduce + (map #(- charge-factor (count %)) values)))
        size-table)
     charge-factor))

(defn hash-function
  "Hash KEY into a slot."
  [{:keys [size-table]} key]
  (mod key size-table))

(defn- set-value [ht key data]
  (let [slot (into [data] (get-in ht [:values key] []))]
    (-> ht
        (assoc-in [:values key] slot)
        (assoc-in [:keys key] slot))))

(defn- linear-probe [{:keys [values] :as ht} key]
  (loop [new-key (hash-function ht (inc key))]
    (if (and (some? (nth values new-key)) (not= (nth values new-key) key))
      (if (some nil? values)
        (recur (hash-function ht (inc new-key)))
        nil)
      new-key)))

(defn- collision-resolution [{:keys [values charge-factor] :as ht} key _data]
  (if-not (and (= (count (nth values key)) charge-factor)
               (zero? (count (filter nil? values))))
    key
    (linear-probe ht key)))

(declare insert-data)

(defn- rehashing [{:keys [values size-table] :as ht}]
  (let [survivors (vec (mapcat identity (filter some? values)))
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
