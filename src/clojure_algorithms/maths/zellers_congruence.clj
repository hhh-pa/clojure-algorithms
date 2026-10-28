(ns clojure-algorithms.maths.zellers-congruence
  "Port of maths/zellers_congruence.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare zeller)

(defn zeller "| Zellers Congruence Algorithm\n| Find the day of the week for nearly any Gregorian or Julian calendar date\n\n>>> zeller('01-31-2010')\n'Your date 01-31-2010, is a Sunday!'\n\nValidate out of range month:\n\n>>> zeller('13-31-2010')\nTraceback (most recent call last):\n    ...\nValueError: Month must be between 1 - 12\n>>> zeller('.2-31-2010')\nTraceback (most recent call last):\n    ...\nValueError: invalid literal for int() with base 10: '.2'\n\nValidate out of range date:\n\n>>> zeller('01-33-2010')\nTraceback (most recent call last):\n    ...\nValueError: Date must be between 1 - 31\n>>> zeller('01-.4-2010')\nTraceback (most recent call last):\n    ...\nValueError: invalid literal for int() with base 10: '.4'\n\nValidate second separator:\n\n>>> zeller('01-31*2010')\nTraceback (most recent call last):\n    ...\nValueError: Date separator must be '-' or '/'\n\nValidate first separator:\n\n>>> zeller('01^31-2010')\nTraceback (most recent call last):\n    ...\nValueError: Date separator must be '-' or '/'\n\nValidate out of range year:\n\n>>> zeller('01-31-8999')\nTraceback (most recent call last):\n    ...\nValueError: Year out of range. There has to be some sort of limit...right?\n\nTest null input:\n\n>>> zeller()\nTraceback (most recent call last):\n    ...\nTypeError: zeller() missing 1 required positional argument: 'date_input'\n\nTest length of `date_input`:\n\n>>> zeller('')\nTraceback (most recent call last):\n    ...\nValueError: Must be 10 characters long\n>>> zeller('01-31-19082939')\nTraceback (most recent call last):\n    ...\nValueError: Must be 10 characters long"
  [& _args]
  ;; stub: original requires method .weekday; pure-Clojure port not applicable
  (throw (ex-info "requires method .weekday" {:type :UnsupportedOperation})))

;; NOTE: zeller stubbed (method .weekday)
