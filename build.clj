(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def lib 'io.github.hhh-pa/clojure-algorithms)
(def version "0.1.0-SNAPSHOT")
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn test [_]
  (let [{:keys [exit]} (b/process {:command-args ["clojure" "-M:test"]
                                   :dir "."})]
    (when-not (zero? exit)
      (throw (ex-info "Tests failed" {:exit exit})))))

(defn ci [_]
  (test nil)
  (jar nil))