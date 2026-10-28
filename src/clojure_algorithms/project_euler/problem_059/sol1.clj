(ns clojure-algorithms.project-euler.problem-059.sol1
  "Euler Problem 59: decrypt an XOR-encrypted message whose key is three lower
  case characters, knowing the plain text contains common English words, and
  sum the ASCII values of the original text. The solver takes the cipher data
  (comma-separated ASCII codes) as a parameter instead of a file name.")

(def valid-chars
  "Letters, digits, punctuation and whitespace: the characters a correct
  decryption may contain."
  (str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
       "0123456789"
       "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
       " \t\n\r" (char 0x0C) (char 0x0B)))

(def lowercase-ints
  "ASCII codes of a-z, the possible key characters."
  (mapv int "abcdefghijklmnopqrstuvwxyz"))

(def ^:private valid-ints (set (map int valid-chars)))

(def common-words
  "Common English words used to narrow down candidate decryptions."
  ["the" "be" "to" "of" "and" "in" "that" "have"])

(defn try-key
  "Decrypts ciphertext (a sequence of ASCII codes) with the 3-character key (a
  sequence of ASCII codes). Returns the decoded string, or nil if any decoded
  character is invalid."
  [ciphertext key]
  (let [sb (StringBuilder.)]
    (loop [ks (cycle key) cs (seq ciphertext)]
      (if (empty? cs)
        (.toString sb)
        (let [decoded (bit-xor (first cs) (first ks))]
          (if (contains? valid-ints decoded)
            (do (.append sb (char decoded))
                (recur (rest ks) (rest cs)))
            nil))))))

(defn filter-valid-chars
  "Tests all 3-character lower case keys against ciphertext and returns the
  decryptions containing only valid characters."
  [ciphertext]
  (into [] (comp (map (fn [key] (try-key ciphertext key)))
                 (filter some?))
        (for [a lowercase-ints b lowercase-ints c lowercase-ints] [a b c])))

(defn filter-common-word
  "Narrows possible decryptions down to those containing common-word
  (case-insensitive)."
  [possibles common-word]
  (filterv (fn [possible]
             (.contains (.toLowerCase possible)
                        (.toLowerCase common-word)))
           possibles))

(defn solution-from-ciphertext
  "Decrypts the ciphertext code sequence by narrowing candidates with common
  words, and returns the ASCII sum of the decoded text."
  [ciphertext]
  (let [possibles (filter-valid-chars ciphertext)
        decoded (loop [possibles possibles words common-words]
                  (if (empty? words)
                    possibles
                    (let [narrowed (filter-common-word possibles (first words))]
                      (if (= 1 (count narrowed))
                        narrowed
                        (recur narrowed (rest words))))))]
    (reduce + (map int (first decoded)))))

(defn solution
  "Decrypts data, the comma-separated ASCII codes of the cipher text, and
  returns the sum of the ASCII values of the original text."
  [data]
  (solution-from-ciphertext
   (mapv #(Integer/parseInt %) (.split (.trim data) ","))))
