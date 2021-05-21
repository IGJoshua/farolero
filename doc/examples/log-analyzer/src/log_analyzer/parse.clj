(ns log-analyzer.parse
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [farolero.core :as far :refer [handler-bind handler-case restart-case
                                  wrap-exceptions translate-exceptions values]])
  (:import
   (java.time format.DateTimeFormatter LocalDateTime)))

(def log-line-regex #"^(\S+) +\[(.*?)\] +(\S+) +(\S+) +- +(.*)$")

(defn parse-log-line
  [s]
  (restart-case
      (if-let [[date thread level ns message] (next (re-matches log-line-regex s))]
        (translate-exceptions [Exception (fn [_] (values ::invalid-log-entry :log-line s))]
          {:date (LocalDateTime/parse date DateTimeFormatter/ISO_DATE_TIME)
           :thread thread
           :level (keyword (str/lower-case level))
           :ns (symbol ns)
           :message message})
        (far/error ::invalid-log-entry
                   :log-line s))
    (::far/continue [])
    (::far/use-value [v] v)))

(defn parse-log-file
  [lines]
  (keep parse-log-line lines))
