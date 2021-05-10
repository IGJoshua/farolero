(ns farolero.build
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :refer [sh]])
  (:refer-clojure :exclude [compile]))

(defn- javac
  [file out-dir opts]
  (apply sh "javac" "-d" (.getCanonicalPath out-dir) (concat opts [(.getCanonicalPath file)])))

(defn compile
  [args]
  (let [{:keys [source-dirs compiler-options output-dir]} args
        output-file (io/file output-dir)
        java-files (mapcat file-seq (map io/file source-dirs))]
    (doseq [file java-files
            :when (.isFile file)]
      (javac file output-file compiler-options)))
  (shutdown-agents))
