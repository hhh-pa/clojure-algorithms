(ns clojure-algorithms.machine-learning.q-learning
  "Q-Learning is a widely-used model-free algorithm in reinforcement learning that\nlearns the optimal action-value function Q(s, a), which tells an agent the expected\nutility of taking action a in state s and then following the optimal policy after.\nIt is able to find the best policy for any given finite Markov decision process (MDP)\nwithout requiring a model of the environment.\n\nSee: [https://en.wikipedia.org/wiki/Q-learning](https://en.wikipedia.org/wiki/Q-learning)"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare get-q-value get-best-action choose-action update get-policy reset-env get-available-actions-env step-env run-q-learning State LEARNING-RATE DISCOUNT-FACTOR EPSILON EPSILON-DECAY EPSILON-MIN q-table SIZE GOAL current-state)

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
