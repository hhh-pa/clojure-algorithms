(ns clojure-algorithms.data-structures.stacks.postfix-evaluation
  "Port of data_structures/stacks/postfix_evaluation.py.
  Postfix evaluation with unary +/- support.")

(def unary-op-symbols #{"-" "+"})

(def operators
  {"^" #(Math/pow (double %1) (double %2))
   "*" * "/" / "+" + "-" -})

(defn parse-token
  "Parse TOKEN to a double, or return the operator string."
  [token]
  (if (contains? operators (str token))
    (str token)
    (try (Double/parseDouble (str token))
         (catch NumberFormatException _
           (throw (ex-info (str token " is neither a number nor a valid operator")
                           {:type :value-error}))))))

(defn evaluate
  "Evaluate postfix TOKENS; returns a double (0 for empty input)."
  ([post-fix] (evaluate post-fix false))
  ([post-fix verbose]
   (if (empty? post-fix)
     0
     (let [valid (mapv parse-token post-fix)]
       (when verbose
         (println " Symbol  |    Action    | Stack"))
       (let [stack
             (reduce (fn [stack x]
                       (if-not (contains? operators (str x))
                         (do
                           (when verbose
                             (println (format "%8s | %-12s | %s" x (str "push(" x ")")
                                              (conj stack x))))
                           (conj stack x))
                         (if (and (contains? unary-op-symbols (str x))
                                  (< (count stack) 2))
                           (let [b (peek stack)
                                 stack (pop stack)
                                 b (if (= (str x) "-") (- b) b)]
                             (when verbose
                               (println (format "%8s | %-12s | %s" "" (str "pop(" b ")")
                                                stack))
                               (println (format "%8s | %-12s | %s" x
                                                (str "push(" x b ")")
                                                (conj stack b))))
                             (conj stack b))
                           (let [b (peek stack)
                                 stack (pop stack)
                                 a (peek stack)
                                 stack (pop stack)
                                 r ((get operators (str x)) a b)]
                             (when verbose
                               (println (format "%8s | %-12s | %s" x
                                                (str "push(" a x b ")")
                                                (conj stack r))))
                             (conj stack r)))))
                     [] valid)]
         (when-not (= 1 (count stack))
           (throw (ex-info "Input is not a valid postfix expression"
                           {:type :arithmetic-error})))
         (double (first stack)))))))
