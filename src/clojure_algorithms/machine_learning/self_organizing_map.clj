(ns clojure-algorithms.machine-learning.self-organizing-map
  "https://en.wikipedia.org/wiki/Self-organizing_map"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare main make-SelfOrganizingMap SelfOrganizingMap-get-winner SelfOrganizingMap-update)

(defrecord SelfOrganizingMap [])

(defn make-SelfOrganizingMap
  "Constructor for SelfOrganizingMap."
  ([] (map->SelfOrganizingMap {})))

(defn SelfOrganizingMap-get-winner "Compute the winning vector by Euclidean distance\n\n>>> SelfOrganizingMap().get_winner([[1, 2, 3], [4, 5, 6]], [1, 2, 3])\n1"
  [self weights sample] (let [d0 (atom nil) d1 (atom nil) i (atom nil)] (try (reset! d0 0.0) (reset! d1 0.0) (doseq [__i1 (py-iter (range (count sample)))] (reset! i __i1) (swap! d0 py-add (py-pow (py-sub (py-get sample @i) (py-get (py-get weights 0) @i)) 2)) (swap! d1 py-add (py-pow (py-sub (py-get sample @i) (py-get (py-get weights 1) @i)) 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-gt @d0 @d1)) 0 1)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn SelfOrganizingMap-update "Update the winning vector.\n\n>>> SelfOrganizingMap().update([[1, 2, 3], [4, 5, 6]], [1, 2, 3], 1, 0.1)\n[[1, 2, 3], [3.7, 4.7, 6]]"
  [self weights sample j alpha] (let [weights (atom weights)] (let [i (atom nil)] (try (doseq [__i1 (py-iter (range (count @weights)))] (reset! i __i1) (swap! weights (fn [__c] (py-assoc-in __c [j @i] (py-add (py-get-in __c [j @i]) (py-mul alpha (py-sub (py-get sample @i) (py-get (py-get @weights j) @i)))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @weights})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn main
  [] (let [--v (atom nil) alpha (atom nil) epochs (atom nil) j (atom nil) sample (atom nil) self-organizing-map (atom nil) training-samples (atom nil) weights (atom nil) winner (atom nil)] (reset! training-samples [[1 1 0 0] [0 0 0 1] [1 0 0 0] [0 0 1 1]]) (reset! weights [[0.2 0.6 0.5 0.9] [0.8 0.4 0.7 0.3]]) (reset! self-organizing-map (make-SelfOrganizingMap)) (reset! epochs 3) (reset! alpha 0.5) (doseq [__i1 (py-iter (range @epochs))] (reset! --v __i1) (doseq [__i2 (py-iter (range (count @training-samples)))] (reset! j __i2) (reset! sample (py-get @training-samples @j)) (reset! winner (SelfOrganizingMap-get-winner self-organizing-map @weights @sample)) (reset! weights (py-dict-update @self-organizing-map @weights {})))) (reset! sample [0 0 0 1]) (reset! winner (SelfOrganizingMap-get-winner self-organizing-map @weights @sample)) (println (clojure.string/join " " [(py-str (str "Clusters that the test sample belongs to : " (py-str @winner)))])) (println (clojure.string/join " " [(py-str (str "Weights that have been trained : " (py-str @weights)))])) nil))
