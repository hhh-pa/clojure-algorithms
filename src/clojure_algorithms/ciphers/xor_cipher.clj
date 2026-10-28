(ns clojure-algorithms.ciphers.xor-cipher
  "XOR cipher over characters.")

(defn- effective-key
  [key]
  (mod (if (zero? key) 1 key) 256))

(defn encrypt
  "XORs content with key, returning a char vector."
  [content key]
  (let [k (effective-key key)]
    (mapv #(char (bit-xor (int %) k)) content)))

(defn decrypt
  "XORs content with key, returning a char vector."
  [content key]
  (encrypt content key))

(defn encrypt-string
  "XORs content with key, returning a string."
  ([content] (encrypt-string content 0))
  ([content key]
   (apply str (encrypt content key))))

(defn decrypt-string
  "XORs content with key, returning a string."
  ([content] (decrypt-string content 0))
  ([content key]
   (apply str (decrypt content key))))

(defn encrypt-file
  "Encrypts a text file line by line into encrypt.out."
  ([file] (encrypt-file file 0))
  ([file key]
   (try (with-open [r (clojure.java.io/reader file)
                    w (clojure.java.io/writer "encrypt.out")]
          (doseq [line (line-seq r)]
            (.write ^java.io.Writer w (encrypt-string (str line "\n") key))))
        true
        (catch java.io.IOException _ false))))

(defn decrypt-file
  "Decrypts a text file line by line into decrypt.out."
  [file key]
  (try (with-open [r (clojure.java.io/reader file)
                   w (clojure.java.io/writer "decrypt.out")]
         (doseq [line (line-seq r)]
           (.write ^java.io.Writer w (decrypt-string (str line "\n") key))))
       true
       (catch java.io.IOException _ false)))
