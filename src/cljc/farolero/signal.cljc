(ns farolero.signal
  (:require
   [clojure.spec.alpha :as s]
   [farolero.protocols :refer [Jump]])
  #?(:bb (:import
          (clojure.lang ExceptionInfo)
          (java.lang Error))
     :clj (:import
           (farolero.signal Signal))))

#?(:bb (defrecord Signal [target args])
   :cljs (defrecord Signal [target args]))

(defn make-signal
  [target args]
  ;; Babashka can't use custom Java classes but must throw a Java exception.
  ;; Attach Signal to ExceptionInfo and use as cause in Error to propagate to block*,
  ;; and unwrap in that function.
  #?(:bb (->> (->Signal target args)
              (ExceptionInfo. "farolero.signal")
              (Error. "farolero.signal"))
     :clj (Signal. target args)
     :cljs (->Signal target args)))
(s/fdef make-signal
  :args (s/cat :target (s/or :keyword keyword?
                             :internal-integer integer?)
               :args (s/coll-of any?)))

(extend-protocol Jump
  Signal
  (args [signal]
    (.-args signal))
  (is-target? [signal v]
    (= (.-target signal) v)))
