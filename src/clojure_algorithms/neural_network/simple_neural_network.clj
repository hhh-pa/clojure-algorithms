(ns clojure-algorithms.neural-network.simple-neural-network
  "Minimal single-weight feedforward neural network.

  Trains one weight with forward propagation and a sigmoid
  nonlinearity. See https://en.wikipedia.org/wiki/Feedforward_neural_network")

(def initial-value
  "Fixed input used by the training loop."
  0.02)

(defn sigmoid-function
  "Sigmoid of `value`; with `deriv` true returns the sigmoid
  derivative value * (1 - value) instead."
  ([value] (sigmoid-function value false))
  ([value deriv]
   (if deriv
     (* value (- 1 value))
     (/ 1.0 (+ 1.0 (Math/exp (- (double value))))))))

(defn forward-propagation
  "Trains a single random weight for `number-propagations` steps
  toward `expected` and returns the scaled output (0-100 range).
  An explicit `rng` (java.util.Random) may be supplied for tests."
  ([expected number-propagations]
   (forward-propagation expected number-propagations (java.util.Random.)))
  ([expected number-propagations rng]
   (let [weight (atom (double (- (* 2 (inc (.nextInt rng 100))) 1)))
         layer (atom 0.0)]
     (dotimes [_ number-propagations]
       (let [l1 (sigmoid-function (* initial-value @weight))
             err (- (/ expected 100.0) l1)
             delta (* err (sigmoid-function l1 true))]
         (reset! layer l1)
         (swap! weight + (* initial-value delta))))
     (* @layer 100.0))))
