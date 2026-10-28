(ns clojure-algorithms.project-euler.problem-019.sol1
  "Project Euler Problem 19: counting Sundays.

  You are given the following information, but you may prefer to do some
  research for yourself.

  1 Jan 1900 was a Monday.
  Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.

  A leap year occurs on any year evenly divisible by 4, but not on a
  century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth
  century (1 Jan 1901 to 31 Dec 2000)?")

(def ^:private days-per-month
  "Days in each month of a non-leap year."
  [31 28 31 30 31 30 31 31 30 31 30 31])

(defn- leap-year?
  "Returns true when year is a leap year."
  [year]
  (or (and (zero? (mod year 4)) (not (zero? (mod year 100))))
      (zero? (mod year 400))))

(defn solution
  "Returns the number of Sundays that fall on the first of the month
  during the twentieth century (1 Jan 1901 to 31 Dec 2000)."
  []
  (loop [day 6 month 1 year 1901 sundays 0]
    (if (>= year 2001)
      sundays
      (let [day (+ day 7)
            leap (leap-year? year)
            [day month]
            (cond (and leap (> day (nth days-per-month (dec month)))
                       (not= month 2))
                  [(- day (nth days-per-month (dec month))) (inc month)]
                  (and leap (> day 29) (= month 2))
                  [(- day 29) (inc month)]
                  (and (not leap) (> day (nth days-per-month (dec month))))
                  [(- day (nth days-per-month (dec month))) (inc month)]
                  :else [day month])
            [year month] (if (> month 12) [(inc year) 1] [year month])]
        (recur day month year
               (if (and (< year 2001) (= day 1)) (inc sundays) sundays))))))
