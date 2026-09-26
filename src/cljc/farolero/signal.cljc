(ns farolero.signal
  (:require
   [clojure.spec.alpha :as s]
   [farolero.protocols :refer [Jump]])
  #?(:jolt (:import
             (java.lang Error))
     :bb (:import
          (clojure.lang ExceptionInfo)
          (java.lang Error))
     :clj (:import
           (farolero.signal Signal))))

#?(:jolt (defrecord Signal [target args])
   :bb (defrecord Signal [target args])
   :cljs (defrecord Signal [target args]))

(defn make-signal
  [target args]
  ;; Jolt and Babashka wrap the record in Error so ordinary Exception catches
  ;; do not intercept Farolero's non-local control flow.
  #?(:jolt (Error. "farolero.signal"
                   (ex-info "farolero.signal"
                            {:farolero.signal/jump (->Signal target args)}))
     :bb (->> (->Signal target args)
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
