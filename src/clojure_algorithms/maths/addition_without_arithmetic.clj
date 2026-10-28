(ns clojure-algorithms.maths.addition-without-arithmetic
  "Illustrate how to add the integer without arithmetic operation\nAuthor: suraj Kumar\nTime Complexity: 1\nhttps://en.wikipedia.org/wiki/Bitwise_operation"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare add)

(defn add "Add two integers using bitwise operations instead of arithmetic operators.\n- XOR (^) to add bits without carrying\n- AND (&) to calculate carry bits\n- Left shift (<<) to move the carry to the correct position\n\nExamples:\n>>> add(3, 5)\n8\n>>> add(13, 5)\n18\n>>> add(-7, 2)\n-5\n>>> add(0, -7)\n-7\n>>> add(-321, 1)\n-320"
  [first-v second-v] (let [first-v (atom first-v) second-v (atom second-v)] (let [c (atom nil)] (try (while (py-truthy (not (py-eq @second-v 0))) (reset! c (py-band @first-v @second-v)) (swap! first-v py-bxor @second-v) (reset! second-v (.shiftLeft (.toBigInteger (bigint @c)) (int 1)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @first-v})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
