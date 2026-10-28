(ns clojure-algorithms.other.password
  "Random password generation and strength checking (pure logic only).

  Port of TheAlgorithms/Python `other/password.py`.
  The interactive stdin-based `main` is intentionally not ported.")

(def ^:private lower "abcdefghijklmnopqrstuvwxyz")
(def ^:private upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def ^:private digits "0123456789")
(def ^:private punctuation "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
(def ^:private letters (str lower upper))

(def ^:private secure-random (java.security.SecureRandom.))

(defn- secure-choice
  "Cryptographically random character from string `chars`."
  [chars]
  (.charAt chars (.nextInt secure-random (.length chars))))

(defn password-generator
  "Random password of `length` chars (default 8). Non-positive lengths
  yield the empty string, as in the original."
  ([] (password-generator 8))
  ([length]
   (apply str (repeatedly (max 0 length)
                          #(secure-choice (str letters digits punctuation))))))

(defn random-chars
  "Random string of `n` chars drawn from `chars`."
  [chars n]
  (apply str (repeatedly (max 0 n) #(secure-choice chars))))

(defn alternative-password-generator
  "Password of total length `n` that always includes `chars-incl`,
  padded with a mix of letters, digits, and punctuation."
  [chars-incl n]
  (let [i (- n (count chars-incl))
        quotient (quot i 3)
        remainder (mod i 3)
        chars (str chars-incl
                   (random-chars letters (+ quotient remainder))
                   (random-chars digits quotient)
                   (random-chars punctuation quotient))]
    (apply str (shuffle (vec chars)))))

(defn strong-password?
  "True when `password` is at least `min-length` chars (default 8) and
  contains a lowercase letter, an uppercase letter, a digit, and a
  special character."
  ([password] (strong-password? password 8))
  ([password min-length]
   (boolean
    (and (>= (count password) min-length)
         (some #(>= (.indexOf lower (int %)) 0) password)
         (some #(>= (.indexOf upper (int %)) 0) password)
         (some #(>= (.indexOf digits (int %)) 0) password)
         (some #(>= (.indexOf punctuation (int %)) 0) password)))))
