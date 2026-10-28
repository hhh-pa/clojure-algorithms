(ns clojure-algorithms.ciphers.simple-substitution-cipher
  "Simple substitution cipher over A-Z.")

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn check-valid-key
  "Throws unless key is a permutation of the alphabet."
  [key]
  (when-not (= (sort key) (sort letters))
    (throw (ex-info "Error in the key or symbol set." {:key key}))))

(defn translate-message
  "Translates message with key in encrypt/decrypt mode."
  [key message mode]
  (let [[chars-a chars-b] (if (= mode "decrypt") [key letters] [letters key])]
    (apply str (map (fn [sym]
                      (let [i (.indexOf ^String chars-a
                                        (str (Character/toUpperCase ^char sym)))]
                        (if (neg? i)
                          sym
                          (let [c (nth chars-b i)]
                            (if (Character/isUpperCase ^char sym)
                              (Character/toUpperCase ^char c)
                              (Character/toLowerCase ^char c))))))
                    message))))

(defn encrypt-message
  "Encrypts message with key."
  [key message]
  (translate-message key message "encrypt"))

(defn decrypt-message
  "Decrypts message with key."
  [key message]
  (translate-message key message "decrypt"))

(defn get-random-key
  "Returns a random shuffled alphabet key."
  []
  (apply str (shuffle (seq letters))))
