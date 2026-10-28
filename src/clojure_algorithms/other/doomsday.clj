(ns clojure-algorithms.other.doomsday
  "Doomsday rule for the weekday of a Gregorian date.

  Port of TheAlgorithms/Python `other/doomsday.py`.")

(def ^:private doomsday-leap [4 1 7 4 2 6 4 1 5 3 7 5])
(def ^:private doomsday-not-leap [3 7 7 4 2 6 4 1 5 3 7 5])

(def ^:private week-day-names
  {0 "Sunday" 1 "Monday" 2 "Tuesday" 3 "Wednesday"
   4 "Thursday" 5 "Friday" 6 "Saturday"})

(defn get-week-day
  "Return the weekday name for the Gregorian date `year`-`month`-`day`."
  [year month day]
  (assert (> (count (str year)) 2) "year should be in YYYY format")
  (assert (<= 1 month 12) "month should be between 1 to 12")
  (assert (<= 1 day 31) "day should be between 1 to 31")
  (let [century (quot year 100)
        century-anchor (mod (+ (* 5 (mod century 4)) 2) 7)
        centurian (mod year 100)
        centurian-m (mod centurian 12)
        dooms-day (mod (+ (quot centurian 12) centurian-m
                          (quot centurian-m 4) century-anchor)
                       7)
        leap? (not (or (not (zero? (mod year 4)))
                       (and (zero? centurian) (not (zero? (mod year 400))))))
        day-anchor ((if leap? doomsday-leap doomsday-not-leap) (dec month))
        week-day (mod (+ dooms-day day (- day-anchor)) 7)]
    (week-day-names week-day)))
