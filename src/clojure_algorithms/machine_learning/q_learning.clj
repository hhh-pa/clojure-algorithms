(ns clojure-algorithms.machine-learning.q-learning
  "Q-Learning is a widely-used model-free algorithm in reinforcement learning that\nlearns the optimal action-value function Q(s, a), which tells an agent the expected\nutility of taking action a in state s and then following the optimal policy after.\nIt is able to find the best policy for any given finite Markov decision process (MDP)\nwithout requiring a model of the environment.\n\nSee: [https://en.wikipedia.org/wiki/Q-learning](https://en.wikipedia.org/wiki/Q-learning)"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare get-q-value get-best-action choose-action update get-policy reset-env get-available-actions-env step-env run-q-learning State LEARNING-RATE DISCOUNT-FACTOR EPSILON EPSILON-DECAY EPSILON-MIN q-table SIZE GOAL current-state)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-index-norm py-get py-slice-assign py-assoc-one py-assoc-in py-add py-sub py-repeat-vec py-mul py-neg py-min py-min-by py-max py-max-by py-sorted py-float-str py-quote py-repr py-str py-rng py-rand)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-index-norm [k n]
  (let [i (cond (boolean? k) (if k 1 0)
                (integer? k) (long k)
                :else (throw (ex-info "indices must be integers" {:type :TypeError})))
        ii (if (neg? i) (+ n i) i)]
    (when (or (neg? ii) (>= ii n))
      (throw (ex-info "index out of range" {:type :IndexError})))
    ii))

(defn- py-get [c k]
  (cond (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :getitem c k)]
          (if (identical? r py-no-dunder)
            (if (and (keyword? k) (contains? c k)) (get c k)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r))
        (map? c) (if (contains? c k) (get c k)
                     (throw (ex-info (str "key: " (py-repr k)) {:type :KeyError})))
        (string? c) (str (nth c (py-index-norm k (count c))))
        :else (nth (vec (seq c)) (py-index-norm k (count c)))))

(defn- py-slice-assign [c lo hi step v]
  (if (string? c)
    (throw (ex-info "does not support item assignment" {:type :TypeError}))
    (let [vv (vec c) n (count vv) nv (vec v)
          step (if (nil? step) 1 step)]
      (if (= step 1)
        (let [a (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
              b (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))]
          (vec (concat (subvec vv 0 a) nv (subvec vv b))))
        (let [idxs (if (pos? step)
                     (range (cond (nil? lo) 0 (neg? lo) (max 0 (+ n lo)) :else (min n lo))
                            (cond (nil? hi) n (neg? hi) (max 0 (+ n hi)) :else (min n hi))
                            step)
                     (range (cond (nil? lo) (dec n) (neg? lo) (+ n lo) :else (min (dec n) lo))
                            (cond (nil? hi) -1 (neg? hi) (+ n hi) :else hi)
                            step))]
          (when (not= (count idxs) (count nv))
            (throw (ex-info "attempt to assign sequence of different size" {:type :ValueError})))
          (reduce (fn [acc [i x]] (assoc acc i x)) vv (map vector idxs nv)))))))

(defn- py-assoc-one [c k v]
  (cond (and (vector? k) (= :slice (first k)))
        (py-slice-assign c (nth k 1) (nth k 2) (nth k 3) v)
        (map? c) (assoc c k v)
        (string? c) (throw (ex-info "does not support item assignment" {:type :TypeError}))
        :else (let [vv (vec c) n (count vv)
                    i (cond (boolean? k) (if k 1 0)
                            (integer? k) (long k)
                            :else (throw (ex-info "indices must be integers" {:type :TypeError})))
                    ii (if (neg? i) (+ n i) i)]
                (when (or (neg? ii) (>= ii n))
                  (throw (ex-info "assignment index out of range" {:type :IndexError})))
                (assoc vv ii v))))

(defn- py-assoc-in [c ks v]
  (let [[k & more] ks]
    (if (instance? clojure.lang.IRecord c)
      (if (seq more)
        (assoc c k (py-assoc-in (py-get c k) more v))
        (let [r (py-dunder :setitem c k v)]
          (if (identical? r py-no-dunder)
            (if (keyword? k) (assoc c k v)
                (throw (ex-info "not subscriptable" {:type :TypeError})))
            r)))
      (if (seq more)
        (py-assoc-one c k (py-assoc-in (py-get c k) more v))
        (py-assoc-one c k v)))))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-neg [x]
  (cond (boolean? x) (if x -1 0)
        (number? x) (-' x)
        (instance? clojure.lang.IRecord x)
        (let [r (py-dunder :neg x)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "bad operand for unary -" {:type :TypeError})) r))
        :else (throw (ex-info "bad operand for unary -" {:type :TypeError}))))

(defn- py-min [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt b a) b a)) s)))

(defn- py-min-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "min() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-lt (k b) (k a)) b a)) s)))

(defn- py-max [c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt b a) b a)) s)))

(defn- py-max-by [k c]
  (let [s (seq (py-iter c))]
    (when (nil? s) (throw (ex-info "max() arg is an empty sequence" {:type :ValueError})))
    (reduce (fn [a b] (if (py-gt (k b) (k a)) b a)) s)))

(defn- py-sorted [c]
  (cond (string? c) (vec (sort (map str (seq c))))
        (map? c) (vec (sort (keys c)))
        :else (try (vec (sort c))
                   (catch ClassCastException e
                     (throw (ex-info "not orderable" {:type :TypeError}))))))

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

(def ^:private py-rng (atom (java.util.Random.)))

(defn- py-rand [] (.nextDouble ^java.util.Random @py-rng))

(defn get-q-value "Get Q-value for a given state-action pair.\n\n>>> q_table.clear()\n>>> get_q_value((0, 0), 2)\n0.0"
  [state action] (py-get (py-get @q-table state) action))

(defn get-best-action "Get the action with maximum Q-value in the given state.\n\n>>> q_table.clear()\n>>> q_table[(0, 0)][1] = 0.7\n>>> q_table[(0, 0)][2] = 0.7\n>>> q_table[(0, 0)][3] = 0.5\n>>> get_best_action((0, 0), [1, 2, 3]) in [1, 2]\nTrue"
  [state available-actions] (let [best (atom nil) max-q (atom nil)] (try (when (py-truthy (not (py-truthy available-actions))) (throw (ex-info (py-str "No available actions provided") {:type :ValueError}))) (reset! max-q (py-max (doall (for [a (py-iter available-actions)] (py-get (py-get @q-table state) a))))) (reset! best (vec (for [a (py-iter available-actions) :when (py-truthy (py-eq (py-get (py-get @q-table state) a) @max-q))] a))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (rand-nth (vec @best))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn choose-action "Choose action using epsilon-greedy policy.\n\n>>> q_table.clear()\n>>> old_epsilon = EPSILON\n>>> EPSILON = 0.0\n>>> q_table[(0, 0)][1] = 1.0\n>>> q_table[(0, 0)][2] = 0.5\n>>> result = choose_action((0, 0), [1, 2])\n>>> EPSILON = old_epsilon  # Restore\n>>> result\n1"
  [state available-actions] (try (when (py-truthy (not (py-truthy available-actions))) (throw (ex-info (py-str "No available actions provided") {:type :ValueError}))) (when (py-truthy (py-lt (py-rand) @EPSILON)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (rand-nth (vec available-actions))}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (get-best-action state available-actions)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn update "Perform Q-value update for a transition using the Q-learning rule.\n\nQ(s,a) <- Q(s,a) + alpha * (r + gamma * max_a' Q(s',a') - Q(s,a))\n\n>>> q_table.clear()\n>>> update((0, 0), 1, 1.0, (0, 1), [1, 2], done=True, alpha=0.5, gamma=0.9)\n>>> get_q_value((0, 0), 1)\n0.5"
  ([state action reward next-state next-available-actions] (update state action reward next-state next-available-actions false nil nil))
  ([state action reward next-state next-available-actions done] (update state action reward next-state next-available-actions done nil nil))
  ([state action reward next-state next-available-actions done alpha] (update state action reward next-state next-available-actions done alpha nil))
  ([state action reward next-state next-available-actions done alpha gamma] (let [alpha (atom alpha) gamma (atom gamma)] (let [max-q-next (atom nil) new-q (atom nil) old-q (atom nil) q-table (atom nil)] (reset! alpha (if (py-truthy (some? @alpha)) @alpha @LEARNING-RATE)) (reset! gamma (if (py-truthy (some? @gamma)) @gamma @DISCOUNT-FACTOR)) (reset! max-q-next (if (py-truthy (let [__b1 done] (if (py-truthy __b1) __b1 (not (py-truthy next-available-actions))))) 0.0 (py-max (doall (for [a (py-iter next-available-actions)] (get-q-value next-state a)))))) (reset! old-q (get-q-value state action)) (reset! new-q (py-add (py-mul (py-sub 1 @alpha) @old-q) (py-mul @alpha (py-add reward (py-mul @gamma @max-q-next))))) (swap! q-table (fn [__c] (py-assoc-in __c [state action] @new-q))) nil))))

(defn get-policy "Extract a deterministic policy from the Q-table.\n\n\n>>> q_table.clear()\n>>> q_table[(1, 2)][1] = 2.0\n>>> q_table[(1, 2)][2] = 1.0\n>>> get_policy()[(1, 2)]\n1"
  [] (let [a-dict (atom nil) policy (atom nil) s (atom nil)] (try (reset! policy {}) (doseq [[__i1 __i2] (py-iter (vec @q-table))] (reset! s __i1) (reset! a-dict __i2) (when (py-truthy @a-dict) (swap! policy (fn [__c] (py-assoc-in __c [@s] (py-max-by (fn [a] (py-get @a-dict a)) @a-dict)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @policy})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn reset-env "Reset the environment to initial state.\n\n>>> old_state = current_state\n>>> current_state = (1, 1)  # Simulate non-initial state\n>>> result = reset_env()\n>>> current_state = old_state  # Restore for other tests\n>>> result\n(0, 0)"
  [] (let [current-state (atom nil)] (try (reset! current-state [0 0]) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @current-state})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn get-available-actions-env "Get available actions in the current environment state."
  [] [0 1 2 3])

(defn step-env "Take a step in the environment with the given action."
  [action] (let [current-state (atom nil) done (atom nil) next-state (atom nil) reward (atom nil) x (atom nil) y (atom nil)] (try (let [__d1 (vec @current-state)] (reset! x (nth __d1 0)) (reset! y (nth __d1 1))) (if (py-truthy (py-eq action 0)) (do (reset! x (py-max [0 (py-sub @x 1)]))) (do (if (py-truthy (py-eq action 1)) (do (reset! y (py-min [(py-sub SIZE 1) (py-add @y 1)]))) (do (if (py-truthy (py-eq action 2)) (do (reset! x (py-min [(py-sub SIZE 1) (py-add @x 1)]))) (do (when (py-truthy (py-eq action 3)) (reset! y (py-max [0 (py-sub @y 1)]))))))))) (reset! next-state [@x @y]) (reset! reward (if (py-truthy (py-eq @next-state GOAL)) 10.0 (py-neg 1.0))) (reset! done (py-eq @next-state GOAL)) (reset! current-state @next-state) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@next-state @reward @done]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn run-q-learning "Run Q-Learning on the simple grid world environment."
  [] (let [EPSILON (atom nil) --v (atom nil) a (atom nil) action (atom nil) actions (atom nil) done (atom nil) episodes (atom nil) next-actions (atom nil) next-state (atom nil) policy (atom nil) reward (atom nil) s (atom nil) state (atom nil)] (reset! episodes 200) (doseq [__i1 (py-iter (range @episodes))] (reset! --v __i1) (reset! state (reset-env)) (reset! done false) (while (py-truthy (not (py-truthy @done))) (reset! actions (get-available-actions-env)) (reset! action (choose-action @state @actions)) (let [__d2 (vec (step-env @action))] (reset! next-state (nth __d2 0)) (reset! reward (nth __d2 1)) (reset! done (nth __d2 2))) (reset! next-actions (get-available-actions-env)) (update @state @action @reward @next-state @next-actions @done) (reset! state @next-state)) (reset! EPSILON (py-max [(py-mul @EPSILON EPSILON-DECAY) EPSILON-MIN]))) (reset! policy (get-policy)) (println (clojure.string/join " " [(py-str "Learned Policy (state: action):")])) (doseq [[__i3 __i4] (py-iter (py-sorted (vec @policy)))] (reset! s __i3) (reset! a __i4) (println (clojure.string/join " " [(py-str (str (py-str @s) ": " (py-str @a)))]))) nil))

(def State nil)

(def LEARNING-RATE (atom 0.1))

(def DISCOUNT-FACTOR (atom 0.97))

(def EPSILON (atom 0.2))

(def EPSILON-DECAY 0.995)

(def EPSILON-MIN 0.01)

(def q-table (atom {}))

(def SIZE 4)

(def GOAL [(py-sub SIZE 1) (py-sub SIZE 1)])

(def current-state (atom [0 0]))
