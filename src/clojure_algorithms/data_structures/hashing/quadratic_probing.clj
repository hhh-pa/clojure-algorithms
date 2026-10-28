(ns clojure-algorithms.data-structures.hashing.quadratic-probing
  "Port of data_structures/hashing/quadratic_probing.py.
  Hash table with quadratic-probing collision resolution."
  (:require [clojure-algorithms.data-structures.hashing.number-theory.prime-numbers
             :refer [next-prime]]))

(defn make-quadratic-probing
  "Quadratic-probing table with SIZE-TABLE slots."
  ([size-table] (make-quadratic-probing size-table 1 0.75))
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

(defn- collision-resolution [{:keys [values lim-charge] :as ht} key _data]
  (loop [i 1 new-key (hash-function ht (+ key (* i i)))]
    (if (and (some? (nth values new-key)) (not= (nth values new-key) key))
      (let [i (inc i)
            new-key (when-not (>= (balanced-factor ht) lim-charge)
                      (hash-function ht (+ key (* i i))))]
        (if (nil? new-key) nil (recur i new-key)))
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
