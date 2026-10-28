(ns clojure-algorithms.other.gauss-easter
  "Gauss's algorithm for the Gregorian Easter date.

  Port of TheAlgorithms/Python `other/gauss_easter.py`.
  Returns a `java.time.LocalDate`. Integer arithmetic is used throughout,
  matching the documented Gauss algorithm and the Python doctests."
  (:import [java.time LocalDate]))

(defn gauss-easter
  "Return Easter Sunday of `year` as a java.time.LocalDate."
  [year]
  (let [metonic-cycle (mod year 19)
        julian-leap-year (mod year 4)
        non-leap-year (mod year 7)
        leap-day-inhibits (quot year 100)
        lunar-orbit-correction (quot (+ 13 (* 8 leap-day-inhibits)) 25)
        leap-day-reinstall-number (quot leap-day-inhibits 4)
        secular-moon-shift (mod (- (+ 15 leap-day-inhibits)
                                   (+ lunar-orbit-correction
                                      leap-day-reinstall-number))
                                30)
        century-starting-point (mod (- (+ 4 leap-day-inhibits)
                                       leap-day-reinstall-number)
                                    7)
        days-to-add (mod (+ (* 19 metonic-cycle) secular-moon-shift) 30)
        days-from-phm-to-sunday (mod (+ (* 2 julian-leap-year)
                                        (* 4 non-leap-year)
                                        (* 6 days-to-add)
                                        century-starting-point)
                                     7)]
    (cond
      (and (= days-to-add 29) (= days-from-phm-to-sunday 6))
      (LocalDate/of year 4 19)
      (and (= days-to-add 28) (= days-from-phm-to-sunday 6))
      (LocalDate/of year 4 18)
      :else
      (.plusDays (LocalDate/of year 3 22)
                 (long (+ days-to-add days-from-phm-to-sunday))))))
