(ns clojure-algorithms.maths.quadratic-equations-complex-numbers
  "Port of maths/quadratic_equations_complex_numbers.py"
  (:require [clojure.string :as str]))

(declare quadratic-roots main)

(declare py-float-str py-quote py-repr py-str)

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

(defn quadratic-roots "Given the numerical coefficients a, b and c,\ncalculates the roots for any quadratic equation of the form ax^2 + bx + c\n\n>>> quadratic_roots(a=1, b=3, c=-4)\n(1.0, -4.0)\n>>> quadratic_roots(5, 6, 1)\n(-0.2, -1.0)\n>>> quadratic_roots(1, -6, 25)\n((3+4j), (3-4j))"
  [& _args]
  ;; stub: original requires complex-attr; pure-Clojure port not applicable
  (throw (ex-info "requires complex-attr" {:type :UnsupportedOperation})))

(defn main
  [] (let [solution1 (atom nil) solution2 (atom nil)] (let [__d1 (vec (quadratic-roots nil nil nil))] (reset! solution1 (nth __d1 0)) (reset! solution2 (nth __d1 1))) (println (clojure.string/join " " [(py-str (str "The solutions are: " (py-str @solution1) " and " (py-str @solution2)))])) nil))

;; NOTE: quadratic_roots stubbed (complex-attr)
