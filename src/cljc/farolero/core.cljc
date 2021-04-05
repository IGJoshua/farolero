(ns farolero.core
  "Common Lisp style handlers and restarts for errors."
  (:require
   [farolero.proto :as p :refer [Jump args is-target?]]
   [clojure.spec.alpha :as s]
   #?@(:clj ([net.cgrand.macrovich :as macros]
             [clojure.stacktrace :as st])
       :cljs ([farolero.signal :refer [->Signal]])))
  #?(:clj
     (:import
      [farolero.signal Signal])
     :cljs
     (:require-macros
      [net.cgrand.macrovich :as macros])))

(def ^:dynamic *handlers*
  "Dynamically-bound map of handler types to functions."
  {})
(def ^:dynamic *in-restartable-context*
  "Dynamically-bound boolean stating whether or not restarts are allowed."
  false)

(s/def ::handler-key (s/or :keyword keyword?
                           :class symbol?))

(defmacro handler-bind
  "Runs the `body` with bound signal handlers to recover from errors.
  Bindings are of the form:
  condition-type handler-fn

  The condition-type must be a keyword, or a class name for the object used as
  the condition. This is tested with `isa?`, permitting the use of Clojure
  hierarchies. If it is a keyword, it's recommended to be namespaced. If it is a
  class name, it checks if the [[type]] of the condition matches the
  condition-type.

  The handler-fn is a function of at least one argument. The first argument is
  the condition which was signalled, additional arguments are passed from the
  rest arguments used when signalling.

  If the handler returns normally, then additional handlers which apply to the
  condition type are run in order of most specific to least until no more are
  left. If all applicable handlers return normally, then signal function will
  return normally as well."
  {:arglists '([[bindings*] exprs*])
   :style/indent [:defn]}
  [bindings & body]
  (let [bindings (reduce-kv (fn [m k v]
                              (assoc m k (cons 'list (map second v))))
                            {}
                            (group-by first (partition 2 bindings)))]
    `(binding [*handlers* (merge-with into *handlers* ~bindings)]
       ~@body)))
(s/fdef handler-bind
  :args (s/cat :bindings (s/and (s/* (s/cat :key ::handler-key
                                            :handler any?))
                                vector?)
               :body (s/* any?)))

(defn make-jump
  "INTERNAL: Constructs an implementation of [[Jump]]."
  [target args]
  (macros/case
      :clj (Signal. target args)
      :cljs (->Signal target args)))
(s/fdef make-jump
  :args (s/cat :target keyword?
               :args (s/coll-of any?))
  :ret keyword?)

(macros/deftime
  (defn jump-factory
    "INTERNAL: Constructs a function body which throws to the passed `target`."
    [target]
    `(fn [& args#]
       (throw (make-jump ~target args#))))
  (s/fdef jump-factory
    :args (s/cat :target keyword?)))

(defn make-jump-target
  "INTERNAL: Constructs a new [[gensym]]med keyword used as the target of a jump."
  []
  (keyword "farolero.core" (name (gensym "jump-target"))))
(s/fdef make-jump-target
  :ret keyword?)

(defmacro handler-case
  "Runs the `expr` with signal handlers bound, returning the value from the handler on signal.
  Bindings match the form from [[handler-bind]].

  If a condition handled by one of this binding's clauses is signalled, the
  stack is immediately unwound out of the context of `expr`, and then the
  handler bound has its code run, with its return value used as a replacement
  for the return value of the entire `expr`."
  {:arglists '([expr bindings*])
   :style/indent [:defn]}
  [expr & bindings]
  (let [bindings (into {} (partition-all 2) bindings)
        targets (zipmap (keys bindings) (repeatedly (count bindings) make-jump-target))
        factories (into {}
                        (map #(vector % (jump-factory (get targets %))))
                        (keys bindings))
        clauses (map #(vector (get targets %) (get bindings %))
                     (keys bindings))
        e (gensym)]
    `(try
       (handler-bind [~@(mapcat identity factories)]
         ~expr)
       (catch ~(macros/case :clj 'farolero.signal.Signal :cljs 'js/Object) ~e
         ~(let [src `(apply (condp #(is-target? %2 %1) ~e
                              ~@(mapcat identity clauses)
                              (throw ~e))
                            (args ~e))]
            (macros/case
                :clj src
                :cljs `(if (satisfies? Jump ~e)
                         ~src
                         (throw ~e))))))))
(s/fdef handler-case
  :args (s/cat :expr any?
               :bindings (s/* (s/spec (s/cat :key ::handler-key
                                             :handler-fn any?)))))

(def ^:dynamic *restarts*
  "Dynamically-bound map of restart names to functions."
  {})

(defmacro restart-bind
  "Runs the `body` with bound restarts.
  Within the dynamic scope of the `body`, [[invoke-restart]] may be called with
  any of the bound restart names. This includes inside handlers bound further up
  the stack.

  Each binding clause is of the following form:
  restart-name restart-fn

  The restart-name can be any key for a map, but it is recommended to use a
  namespaced keyword.

  The restart-fn is a function of zero or more arguments, provided by rest
  arguments on the call to [[invoke-restart]]. The function returns normally."
  {:arglists '([[bindings*] exprs*])
   :style/indent [:defn]}
  [bindings & body]
  `(binding [*in-restartable-context* true
             *restarts* (merge *restarts* ~(apply hash-map bindings))]
     ~@body))
(s/fdef restart-bind
  :args (s/cat :bindings (s/and (s/* (s/cat :key keyword?
                                            :restart any?))
                                vector?)
               :body (s/* any?)))

(defmacro restart-case
  "Runs the `expr` with bound restarts, returning a value from the restart on invoke.
  Bindings match [[restart-bind]].

  If one of the restarts bound in this case is invoked then the stack is
  immediately unwound to outside of `expr`, and then the restart is run, with
  its return value used as a replacement for its return value."
  {:arglists '([expr bindings*])
   :style/indent [:defn]}
  [expr & bindings]
  (let [bindings (into {} (partition-all 2) bindings)
        jump-targets (zipmap (keys bindings) (repeatedly (count bindings) make-jump-target))
        jump-factories (into {}
                             (map #(vector % (jump-factory (get jump-targets %))))
                             (keys bindings))
        restart-clauses (into {}
                              (map #(vector (get jump-targets %) (get bindings %)))
                              (keys jump-targets))
        e (gensym)]
    `(try
       (restart-bind [~@(mapcat identity jump-factories)]
         ~expr)
       (catch ~(macros/case :clj 'farolero.signal.Signal :cljs 'js/Object) ~e
         ~(let [src `(apply (condp #(is-target? %2 %1) ~e
                              ~@(mapcat identity restart-clauses)
                              (throw ~e))
                            (args ~e))]
            (macros/case
                :clj src
                :cljs `(if (satisfies? Jump ~e)
                         ~src
                         (throw ~e))))))))
(s/fdef restart-case
  :args (s/cat :expr any?
               :bindings (s/* (s/cat :key keyword?
                                     :restart any?))))

(defn find-handlers
  "Searches the environment for handlers given a particular condition.
  Returns a sequence of handler keys which apply to the given condition, from
  most specific to least. The handler keys consist of keywords and class names.

  Complex hierarchies with multiple parents provide undefined ordering."
  [condition]
  (filter #(or (isa? condition %)
               #?(:clj (and (class? %) (instance? % condition))))
          (sort-by (comp count ancestors) > (keys *handlers*))))
(s/fdef find-handlers
  :args (s/cat :condition any?)
  :ret (s/coll-of ::handler-key))

(defn signal
  "Signals a condition, triggering handlers bound for the condition type.
  Looks for handlers which apply to the given `condition` using
  [[find-handlers]] and then applies them in sequence until they all complete or
  one calls [[invoke-restart]]. If this function returns normally, it will
  return nil."
  [condition & args]
  (run! #(apply % condition args)
        (mapcat *handlers* (find-handlers condition)))
  nil)
(s/fdef signal
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(defn warn
  "Signals a condition, printing a warning to [[*err*]] if not handled.
  Binds a restart called `:farolero.core/muffle-warning`, which can be invoked
  from any handlers to prevent the warning without any additional side effects.
  This restart may be invoked directly by calling [[muffle-warning]].

  See [[signal]]."
  [condition & args]
  (restart-case (do (apply signal condition args)
                    (binding #?(:clj [*out* *err*]
                                :cljs [*print-fn* *print-err-fn*])
                      (if (instance? #?(:clj Throwable
                                        :cljs js/Error)
                                     condition)
                        (println "WARNING:" (pr-str (type condition))
                                 "signalled with arguments:" (apply pr-str args)
                                 "\n" (with-out-str
                                        #?(:clj (st/print-cause-trace condition)
                                           :cljs (pr (.stack condition)))))
                        (println "WARNING:" (pr-str condition)
                                 "signalled with arguments:" (apply pr-str args)))))
    ::muffle-warning (constantly nil))
  nil)
(s/fdef warn
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(def ^:dynamic *error-hook*
  "Dynamically-bound hook called after signalling an [[error]] without a handler."
  nil)

(defn error
  "Signals a condition, invoking [[*error-hook*]] if no handler is found.
  If [[*error-hook*]] is not bound to a function, this throws an [[ex-info]]
  with the message \"Unhandled condition\", and a map of the condition, bound
  handlers at the time of the error, and the arguments passed to the handler in
  the [[ex-data]].

  See [[signal]]."
  [condition & args]
  (apply signal condition args)
  (if-not *error-hook*
    (throw (ex-info "Unhandled condition" {:condition condition
                                           :handlers (keys *handlers*)
                                           :args args}
                    (when (instance? #?(:clj Throwable
                                        :cljs js/Error)
                                     condition)
                      condition)))
    (apply *error-hook* condition args)))
(s/fdef error
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(defn cerror
  "Signals a condition as [[error]], but binds a restart to continue.
  The `:farolero.core/continue` restart is bound for any handlers invoked by
  this error. This restart may be invoked directly by calling [[continue]].

  See [[signal]]."
  [condition & args]
  (restart-case (apply error condition args)
    ::continue (constantly nil)))
(s/fdef cerror
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(defn find-restart
  "Returns `restart-name` if there is a restart bound with this name."
  [restart-name]
  (when (contains? *restarts* restart-name)
    restart-name))
(s/fdef find-restart
  :args (s/cat :restart-name keyword?)
  :ret (s/nilable keyword?))

(defn invoke-restart
  "Calls a restart by the given name with `args`.
  If the restart isn't found, signals a `:farolero.core/control-error`.

  Throws an assertion exception if called outside a restart context.

  See [[restart-bind]], [[restart-case]]."
  [restart-name & args]
  (assert *in-restartable-context* "you must be inside a restartable context to invoke a restart")
  (if-let [restart (find-restart restart-name)]
    (apply (get *restarts* restart) args)
    (error ::control-error
           :type :missing-restart
           :restart-name restart-name
           :available-restarts (keys *restarts*))))
(s/fdef invoke-restart
  :args (s/cat :restart-name keyword?
               :args (s/* any?)))

(defn muffle-warning
  "Can be called from a handler triggered by a call to [[warn]] to silence it."
  []
  (invoke-restart ::muffle-warning))
(s/fdef muffle-warning
  :ret nil?)

(defn continue
  "Can be called from a handler triggered by a call to [[cerror]] to skip it."
  []
  (invoke-restart ::continue))
(s/fdef continue
  :ret nil?)

(defn block*
  "Calls `f`, so that it may be escaped by calling [[return-from]], passing `block-name`.
  This is analogous to Common Lisp's `catch` operator, with [[return-from]]
  being passed a keyword directly replacing `throw`."
  {:style/indent [:defn]}
  [block-name f & args]
  (try (apply f args)
       (catch #?(:clj farolero.signal.Signal
                 :cljs js/Object) e
         (if (and #?(:cljs (satisfies? Jump e))
                  (is-target? e block-name))
           (first (p/args e))
           (throw e)))))
(s/fdef block*
  :args (s/cat :block-name keyword?
               :f ifn?
               :args (s/* any?)))

(defmacro block
  "Constructs a named block which can be escaped by [[return-from]]."
  [block-name & body]
  `(let [~block-name (make-jump-target)]
     (block* ~block-name
       (fn [] ~@body))))
(s/fdef block
  :args (s/cat :block-name symbol?
               :body (s/* any?)))

(defn return-from
  "Performs an early return from a named [[block]]."
  ([block-name] (return-from block-name nil))
  ([block-name value]
   (throw (make-jump block-name (list value)))))
(s/fdef return-from
  :args (s/cat :block-name keyword?
               :value (s/? any?)))
