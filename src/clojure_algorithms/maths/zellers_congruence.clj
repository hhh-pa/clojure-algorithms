(ns clojure-algorithms.maths.zellers-congruence
  "Zellers Congruence Algorithm.
Find the day of the week for nearly any Gregorian or Julian calendar date."
  (:import [java.time LocalDate]))

(def ^:private days
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(def ^:private separators #{"-" "/"})

(defn- parse-int-field
  "Parse an integer field, raising :ValueError with Python's int() message."
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _
      (throw (ex-info (str "invalid literal for int() with base 10: '" s "'")
                      {:type :ValueError})))))

(defn zeller
  "Find the day of the week for a date string in mm-dd-yyyy or mm/dd/yyyy
format. Returns e.g. \"Your date 01-31-2010, is a Sunday!\"."
  ([] (throw (ex-info "zeller() missing 1 required positional argument: 'date_input'"
                      {:type :TypeError})))
  ([date-input]
   (when-not (< 0 (count date-input) 11)
     (throw (ex-info "Must be 10 characters long" {:type :ValueError})))
   (let [m (parse-int-field (subs date-input 0 2))]
     (when-not (< 0 m 13)
       (throw (ex-info "Month must be between 1 - 12" {:type :ValueError})))
     (when-not (contains? separators (subs date-input 2 3))
       (throw (ex-info "Date separator must be '-' or '/'" {:type :ValueError})))
     (let [d (parse-int-field (subs date-input 3 5))]
       (when-not (< 0 d 32)
         (throw (ex-info "Date must be between 1 - 31" {:type :ValueError})))
       (when-not (contains? separators (subs date-input 5 6))
         (throw (ex-info "Date separator must be '-' or '/'" {:type :ValueError})))
       (let [y (parse-int-field (subs date-input 6 10))]
         (when-not (< 45 y 8500)
           (throw (ex-info "Year out of range. There has to be some sort of limit...right?"
                           {:type :ValueError})))
         (let [dt (try
                    (LocalDate/of y m d)
                    (catch Exception _
                      (throw (ex-info "day is out of range for month"
                                      {:type :ValueError}))))]
           (let [[yy mm] (if (<= m 2) [(dec y) (+ m 12)] [y m])
                 ys (str yy)
                 c (parse-int-field (subs ys 0 2))
                 k (parse-int-field (subs ys 2))
                 t (long (- (* 2.6 mm) 5.39))
                 u (quot c 4)
                 v (quot k 4)
                 x (+ d k)
                 z (+ t u v x)
                 w (- z (* 2 c))
                 f (mod w 7)
                 expected (get {1 1 2 2 3 3 4 4 5 5 6 6 7 0}
                               (.getValue (.getDayOfWeek dt)))]
             (when (not= f expected)
               (throw (AssertionError. "The date was evaluated incorrectly. Contact developer.")))
             (str "Your date " date-input ", is a " (days f) "!"))))))))
