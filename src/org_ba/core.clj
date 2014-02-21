(ns org-ba.core
  (:gen-class)
  (require [clojure.tools.reader.edn :as edn]
           [clojure.java.io :as io]))

(defn -main
  "Runs the input/output scripts"
  [& args]
  (println "Running..."))

(defn read-lispstyle-edn
  "Read one s-expression from a file"
  [filename]
  (with-open [rdr (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (edn/read rdr)))
