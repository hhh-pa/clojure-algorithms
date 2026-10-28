(ns clojure-algorithms.ciphers.shuffled-shift-cipher
  "Shuffled shift (modified Caesar) cipher.")

(def ^:private key-options
  (str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
       "0123456789"
       "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
       " \t\n"))

(defn- neg-pos
  [nums]
  (map-indexed (fn [i v] (if (odd? i) (- v) v)) nums))

(defn- passcode-creator
  []
  (vec (repeatedly (+ 10 (rand-int 11))
                   #(rand-nth (str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                   "0123456789")))))

(defn- make-key-list
  [passcode]
  (let [breakpoints (set passcode)]
    (loop [chars (seq key-options) tmp [] out []]
      (if (empty? chars)
        (vec out)
        (let [tmp (conj tmp (first chars))]
          (if (or (contains? breakpoints (first chars))
                  (= (first chars) (last key-options)))
            (recur (rest chars) [] (into out (reverse tmp)))
            (recur (rest chars) tmp out)))))))

(defn- make-shift-key
  [passcode]
  (let [n (reduce + (neg-pos (map int passcode)))]
    (if (pos? n) n (count passcode))))

(defn make-cipher
  "Builds a cipher from a passcode string (random when omitted)."
  ([] (make-cipher (apply str (passcode-creator))))
  ([passcode]
   (let [pc (vec passcode)]
     {:passcode pc :key-list (make-key-list pc) :shift-key (make-shift-key pc)})))

(defn encrypt
  "Encrypts plaintext with the cipher."
  [cipher plaintext]
  (let [{:keys [key-list shift-key]} cipher
        n (count key-list)
        idx (zipmap key-list (range))]
    (apply str (map #(nth key-list (mod (+ (idx %) shift-key) n)) plaintext))))

(defn decrypt
  "Decrypts ciphertext with the cipher."
  [cipher encoded-message]
  (let [{:keys [key-list shift-key]} cipher
        n (count key-list)
        idx (zipmap key-list (range))]
    (apply str (map #(nth key-list (mod (- (idx %) shift-key) n)) encoded-message))))
