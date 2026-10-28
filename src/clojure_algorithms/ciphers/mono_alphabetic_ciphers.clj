(ns clojure-algorithms.ciphers.mono-alphabetic-ciphers
  "Monoalphabetic substitution cipher.")

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn translate-message
  "Translates message with key in :encrypt or :decrypt mode."
  [key message mode]
  (let [chars-a (if (= mode "decrypt") letters key)
        chars-b (if (= mode "decrypt") key letters)]
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
