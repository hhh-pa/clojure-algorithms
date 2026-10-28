(ns clojure-algorithms.machine-learning.q-learning
  "Q-Learning on a simple grid world.

  Model-free reinforcement learning: the agent learns Q(s, a), the expected
  utility of taking action a in state s. States are [x y] grid positions,
  actions are 0: up, 1: right, 2: down, 3: left.

  The Q-table, epsilon and current state are module-level mutable state, as
  in the upstream code.

  Reference: https://en.wikipedia.org/wiki/Q-learning")

(def State
  "Type alias for grid states ([x y] vectors)."
  nil)

(def LEARNING-RATE 0.1)

(def DISCOUNT-FACTOR 0.97)

(def EPSILON
  "Exploration rate, decayed by run-q-learning."
  (atom 0.2))

(def EPSILON-DECAY 0.995)

(def EPSILON-MIN 0.01)

(def q-table
  "Nested map of state -> action -> value. Reads install a 0.0 default,
  matching the upstream defaultdict behaviour."
  (atom {}))

(def SIZE 4)

(def GOAL [3 3])

(def current-state
  (atom [0 0]))

(defn get-q-value
  "Q-value for a state-action pair (0.0 when unseen).

  >>> (do (reset! q-table {}) (get-q-value [0 0] 2))
  0.0"
  [state action]
  (let [v (get-in @q-table [state action] 0.0)]
    (swap! q-table assoc-in [state action] v)
    v))

(defn get-best-action
  "Random choice among the actions tied for maximum Q-value in state.
  Throws :ValueError when no actions are given."
  [state available-actions]
  (when (empty? available-actions)
    (throw (ex-info "No available actions provided" {:type :ValueError})))
  (let [best (let [max-q (apply max (map #(get-q-value state %) available-actions))]
               (vec (filter #(= (get-q-value state %) max-q) available-actions)))]
    (nth best (rand-int (count best)))))

(defn choose-action
  "Epsilon-greedy action choice. Throws :ValueError without actions."
  [state available-actions]
  (when (empty? available-actions)
    (throw (ex-info "No available actions provided" {:type :ValueError})))
  (if (< (rand) @EPSILON)
    (nth available-actions (rand-int (count available-actions)))
    (get-best-action state available-actions)))

(defn update
  "Apply the Q-learning update for one transition:
  Q(s,a) <- (1-alpha)*Q(s,a) + alpha*(r + gamma*max Q(s',a')).

  >>> (do (reset! q-table {})
  ...     (update [0 0] 1 1.0 [0 1] [1 2] true 0.5 0.9)
  ...     (get-q-value [0 0] 1))
  0.5"
  ([state action reward next-state next-available-actions]
   (update state action reward next-state next-available-actions false nil nil))
  ([state action reward next-state next-available-actions done]
   (update state action reward next-state next-available-actions done nil nil))
  ([state action reward next-state next-available-actions done alpha]
   (update state action reward next-state next-available-actions done alpha nil))
  ([state action reward next-state next-available-actions done alpha gamma]
   (let [alpha (if (some? alpha) alpha LEARNING-RATE)
         gamma (if (some? gamma) gamma DISCOUNT-FACTOR)
         max-q-next (if (or done (empty? next-available-actions))
                      0.0
                      (apply max (map #(get-q-value next-state %)
                                      next-available-actions)))
         old-q (get-q-value state action)
         new-q (+ (* (- 1 alpha) old-q)
                  (* alpha (+ reward (* gamma max-q-next))))]
     (swap! q-table assoc-in [state action] new-q)
     nil)))

(defn get-policy
  "Deterministic policy extracted from the Q-table: state -> best action."
  []
  (into {}
        (keep (fn [[s actions]]
                (when (seq actions)
                  ;; first maximal action wins, like Python's max with a key
                  [s (reduce (fn [best a]
                               (if (> (get actions a) (get actions best)) a best))
                             (keys actions))])))
        @q-table))

(defn reset-env
  "Reset the grid to [0 0] and return it."
  []
  (reset! current-state [0 0]))

(defn get-available-actions-env
  "Actions available in the grid world."
  []
  [0 1 2 3])

(defn step-env
  "Move from the current state with action; returns [next-state reward done].
  Reaching the goal pays 10.0, every other step costs 1.0."
  [action]
  (let [[x y] @current-state
        [x y] (case action
                0 [(max 0 (dec x)) y]
                1 [x (min (dec SIZE) (inc y))]
                2 [(min (dec SIZE) (inc x)) y]
                3 [x (max 0 (dec y))]
                [x y])
        next-state [x y]
        done (= next-state GOAL)
        reward (if done 10.0 -1.0)]
    (reset! current-state next-state)
    [next-state reward done]))

(defn run-q-learning
  "Train for 200 episodes with epsilon decay, then print the policy."
  []
  (doseq [_ (range 200)]
    (loop [state (reset-env)
           done false]
      (when-not done
        (let [action (choose-action state (get-available-actions-env))
              [next-state reward done?] (step-env action)]
          (update state action reward next-state
                  (get-available-actions-env) done?)
          (recur next-state done?))))
    (reset! EPSILON (max (* @EPSILON EPSILON-DECAY) EPSILON-MIN)))
  (println "Learned Policy (state: action):")
  (doseq [[s a] (sort (get-policy))]
    (println (str s ": " a))))
