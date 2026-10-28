(ns clojure-algorithms.maths.decimal-isolate
  "Isolate the Decimal part of a Number\nhttps://stackoverflow.com/questions/3886402/how-to-get-numbers-after-decimal-point"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare decimal-isolate)

(defn decimal-isolate "Isolates the decimal part of a number.\nIf digitAmount > 0 round to that decimal place, else print the entire decimal.\n>>> decimal_isolate(1.53, 0)\n0.53\n>>> decimal_isolate(35.345, 1)\n0.3\n>>> decimal_isolate(35.345, 2)\n0.34\n>>> decimal_isolate(35.345, 3)\n0.345\n>>> decimal_isolate(-14.789, 3)\n-0.789\n>>> decimal_isolate(0, 2)\n0\n>>> decimal_isolate(-14.123, 1)\n-0.1\n>>> decimal_isolate(-14.123, 2)\n-0.12\n>>> decimal_isolate(-14.123, 3)\n-0.123"
  [number digit-amount] (try (when (py-truthy (py-gt digit-amount 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-round (py-sub number (py-int number nil)) digit-amount)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-sub number (py-int number nil))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
