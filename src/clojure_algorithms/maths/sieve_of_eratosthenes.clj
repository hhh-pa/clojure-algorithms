(ns clojure-algorithms.maths.sieve-of-eratosthenes
  "Sieve of Eratosthones

  The sieve of Eratosthenes is an algorithm used to find prime numbers, less
  than or equal to a given value.
  Illustration:
  https://upload.wikimedia.org/wikipedia/commons/b/b9/Sieve_of_Eratosthenes_animation.gif
  Reference: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

  doctest provider: Bruno Simas Hadlich (https://github.com/brunohadlich)
  Also thanks to Dmitry (https://github.com/LizardWizzard) for finding the problem")

(defn prime-sieve
  "Returns a list with all prime numbers up to n."
  [num]
  (when (<= num 0)
    (throw (ex-info (str num ": Invalid input, please enter a positive integer.")
                    {:type :ValueError})))
  (let [sieve (boolean-array (inc num) true)
        end (int (Math/sqrt num))]
    (loop [start 2 primes []]
      (if (> start end)
        (into primes (filter #(aget sieve %) (range (inc end) (inc num))))
        (if (aget sieve start)
          (do (doseq [i (range (* start start) (inc num) start)]
                (aset sieve i false))
              (recur (inc start) (conj primes start)))
          (recur (inc start) primes))))))
