(ns clojure-algorithms.project-euler.problem-079.sol1
  "Project Euler 79: passcode derivation.

  A common security method used for online banking is to ask the user
  for three random characters from a passcode. Given that the three
  characters are always asked for in order, analyse the logins so as to
  determine the shortest possible secret passcode of unknown length.

  This port takes the logins as a collection (or the file content as a
  string) instead of reading keylog.txt."
  (:require [clojure.string :as str]))

(defn- permutations
  "Lazily returns all permutations of coll."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      (list ())
      (mapcat (fn [i]
                (map #(cons (nth v i) %)
                     (permutations (concat (subvec v 0 i)
                                           (subvec v (inc i))))))
              (range (count v))))))

(defn find-secret-passcode
  "Returns the shortest possible secret passcode (as an integer) for the
  given collection of login strings. Unique characters are tried in
  sorted order so the result is deterministic."
  [logins]
  (let [unique-chars (sort (distinct (mapcat seq logins)))]
    (if-let [found (some (fn [perm]
                           (let [positions (zipmap perm (range))]
                             (when (every? (fn [login]
                                             (< (positions (nth login 0))
                                                (positions (nth login 1))
                                                (positions (nth login 2))))
                                           logins)
                               perm)))
                         (permutations unique-chars))]
      (parse-long (apply str found))
      (throw (ex-info "Unable to find the secret passcode" {})))))

(defn solution
  "Returns the shortest possible secret passcode of unknown length for
  the successful login attempts given in content (one per line)."
  [content]
  (find-secret-passcode (str/split-lines (str/trim content))))
