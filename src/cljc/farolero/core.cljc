(ns farolero.core
  "Common Lisp style handlers and restarts for errors."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   #?@(:clj ([net.cgrand.macrovich :as macros]
             [clojure.stacktrace :as st])
       :cljs ([goog.string :as gstring]
              [goog.string.format]))
   [farolero.protocols :refer [#?(:bb Jump :cljs Jump) args is-target?]]
   [farolero.signal :refer [make-signal]])
  #?(:bb
     (:import
      (java.util.concurrent.atomic AtomicLong)
      (java.lang Error))
     :cljs
     (:require-macros
      [farolero.core :refer [restart-case wrap-exceptions]]
      [net.cgrand.macrovich :as macros])
     :clj
     (:import
      (java.util.concurrent.atomic AtomicLong)))
  (:refer-clojure :exclude [assert]))

;; Automatically load extensions in Clojure
(macros/case :clj
  (do
    (defmacro ^:private load-extension!
      [extension-kw]
      `(def ~(symbol (str (name extension-kw) \?))
         (try (require '~(symbol (str "farolero.extensions." (name extension-kw))))
              true
              (catch Exception _#
                nil))))
    (s/fdef load-extension!
      :args (s/cat :extension-kw simple-keyword?))

    (load-extension! :flow)))

(declare error)

(def ^:dynamic *bound-blocks*
  "A set of blocks that the code is currently in the dynamic scope of."
  #{})

(s/def ::jump-target (s/or :keyword keyword?
                           :internal-integer integer?))

(defn block*
  "Calls `f`, so that it may be escaped by calling [[return-from]], passing `block-name`.
  This is analogous to Common Lisp's `catch` operator, with [[return-from]]
  being passed a keyword directly replacing `throw`."
  {:style/indent 2}
  [block-name f & more]
  (try (let [on-stack? (volatile! true)]
         (try (binding [*bound-blocks* (conj *bound-blocks*
                                             ^{:on-stack? on-stack?}
                                             [#?(:clj (Thread/currentThread)
                                                 :cljs :unsupported)
                                              block-name])]
                (apply f more))
              (finally
                (vreset! on-stack? false))))
       (catch #?(:bb Error
                 :clj farolero.signal.Signal
                 :cljs js/Object) e
         ;; Need to unwrap for Babashka, but retain original Error reference
         ;; to rethrow when not for Farolero. (See comment in `make-signal`.)
         (let [e' #?(:bb (some-> e ex-cause ex-data)
                     :clj e
                     :cljs e)]
           (if
             #_{:clj-kondo/ignore #?(:clj [:single-logical-operand] :cljs [])}
             (and #?(:bb (satisfies? Jump e')
                     :cljs (satisfies? Jump e'))
                  (is-target? e' block-name))
             (first (args e'))
             (throw e))))))
(s/fdef block*
  :args (s/cat :block-name ::jump-target
               :f ifn?
               :more (s/* any?)))

(def ^:private jump-counter
  "An atomic long integer used to identify jump targets across threads."
  (macros/case
    :clj
    #_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-symbol])}
    (AtomicLong. 0)
    :cljs
    #_{:clj-kondo/ignore #?(:clj [:unresolved-namespace] :cljs [])}
    (js/Int32Array. 1)))

(defn make-jump-target
  "INTERNAL: Constructs a new object to be used as the target of a jump."
  []
  #?(:clj (.incrementAndGet ^AtomicLong jump-counter)
     :cljs (.add js/Atomics jump-counter 0 1)))

(macros/deftime
(defmacro block
  "Constructs a named block which can be escaped by [[return-from]]."
  {:style/indent 1}
  [block-name & body]
  (if (keyword? block-name)
    `(block* ~block-name
         (fn []
           (try
             (do ~@body))))
    `(let [~block-name (make-jump-target)]
       (block* ~block-name
           (fn []
             (try
               (do ~@body))))))))
(s/fdef block
  :args (s/cat :block-name (s/or :lexical symbol?
                                 :dynamic keyword?)
               :body (s/* any?)))

(defn return-from
  "Performs an early return from a named [[block]]."
  {:style/indent 1}
  ([block-name] (return-from block-name nil))
  ([block-name value]
   (let [block (*bound-blocks* [#?(:clj (Thread/currentThread)
                                   :cljs :unsupported)
                                block-name])]
     (if (and block @(:on-stack? (meta block)))
       (throw (make-signal block-name (list value)))
       (error #:farolero.control-error
              {::condition-type ::control-error
               :type ::outside-block})))))
(s/fdef return-from
  :args (s/cat :block-name ::jump-target
               :value (s/? any?)))

(def ^:dynamic *in-tagbodies*
  "A set of tagbody blocks in the current dynamic scope."
  #{})

(s/def ::tagbody-args (s/cat :initial-expr (s/* (comp not symbol?))
                             :clauses (s/* (s/cat :clause-tag symbol?
                                                  :clause-body (s/* (comp not symbol?))))))

(s/def ::clause-index number?)

(macros/deftime
(defmacro tagbody
  "Performs the clauses in order, returning nil, allowing [[go]] between clauses.
  Each clause is in the following form:
  tag forms*

  The tag is a symbol naming the clause. Optionally any number of forms may be
  placed before the first tag, and these will execute first, although there is
  no way to jump to them after their execution."
  [& clauses]
  (let [clauses (s/conform ::tagbody-args clauses)
        init (not-empty (:initial-expr clauses))
        clauses (concat (when init
                          [{:clause-tag (gensym)
                            :clause-body init}])
                        (:clauses clauses))
        tags (map :clause-tag clauses)
        target-sym (gensym "target")
        go-targets (map-indexed (fn [idx tag]
                                  [tag
                                   {:jump-target target-sym
                                    :clause-index idx}])
                                tags)
        clauses (map-indexed (fn [idx clause]
                               [idx
                                `(do ~@(:clause-body clause)
                                     ~(inc idx))])
                             clauses)
        end (count clauses)]
    (when (pos? end)
      `(let [~target-sym (make-jump-target)
             ~@(mapcat identity go-targets)]
         (binding [*in-tagbodies* (conj *in-tagbodies* ~target-sym)]
           (loop [control-pointer# 0]
             (let [next-ptr#
                   (block* ~target-sym
                           #(case (long control-pointer#)
                              ~@(mapcat identity clauses)
                              (error #:farolero.control-error
                                     {::condition-type ::control-error
                                      :type ::invalid-clause
                                      :clause-number control-pointer#})))]
               (when (not= next-ptr# ~end)
                 (recur (long next-ptr#)))))))))))
(s/fdef tagbody
  :args ::tagbody-args)

(defn go
  "Jumps to the given `tag` in the surrounding [[tagbody]]."
  [tag]
  (when-not (contains? *in-tagbodies* (:jump-target tag))
    (error #:farolero.control-error
           {::condition-type ::control-error
            :type ::outside-block}))
  (return-from (:jump-target tag) (:clause-index tag)))
(s/fdef go
  :args (s/cat :tag (s/keys :req-un [::jump-target ::clause-index])))

(def ^:dynamic *extra-values*
  "Dynamic variable for returning multiple values up the stack."
  ::unbound)

(macros/deftime
(defmacro multiple-value-bind
  "Binds multiple return values.
  Additional return values can be provided by [[values]]."
  {:style/indent 1}
  [[binding expr] & body]
  `(let [expr-fn# (fn [] ~expr)
         ~binding (if (= ::unbound *extra-values*)
                    (binding [*extra-values* '()]
                      (cons (expr-fn#) *extra-values*))
                    (cons (expr-fn#) *extra-values*))]
     ~@body)))
(s/fdef multiple-value-bind
  :args (s/cat :bindings (s/spec (s/cat :binding any?
                                        :expr any?))
               :body (s/* any?)))

(macros/deftime
(defmacro multiple-value-list
  "Returns the multiple values from `expr` as a list."
  [expr]
  `(multiple-value-bind [ret# ~expr]
       ret#)))
(s/fdef multiple-value-list
  :args (s/cat :expr any?))

(macros/deftime
(defmacro multiple-value-call
  "Calls `f` with all the values returned by each of the `forms`."
  {:style/indent 1}
  [f & forms]
  `(apply ~f
          (mapcat identity
                    ~(cons 'list (map (fn [expr] `(multiple-value-list ~expr)) forms))))))
(s/fdef multiple-value-call
  :args (s/cat :function any?
               :args (s/* any?)))

(macros/deftime
(defmacro values
  "Returns multiple values.
  The first value is the \"true\" return value. Additional values may be bound
  using [[multiple-value-bind]].

  Because of limitations on which values can hold metadata, the additional
  values are not actually associated with the primary return value, and are
  instead held in [[*extra-values*]]. This means if a [[multiple-value-bind]] or
  other methods of getting the extra values is done on a call which does not
  return multiple values, it may \"leak\" multiple values which were returned by
  some call within its dynamic extent but whose value was not returned."
  [value & more]
  `(let [ret# ~value]
     (when-not (= ::unbound *extra-values*)
       (set! *extra-values* ~(cons 'list more)))
       ret#)))
(s/fdef values
  :args (s/cat :value any?
               :more (s/* any?)))

(defn values-list
  "Returns the input list as multiple values."
  [values]
  (when-not (= ::unbound *extra-values*)
    (set! *extra-values* (rest values)))
  (first values))
(s/fdef values-list
  :args (s/cat :values (s/coll-of any?))
  :ret any?)

(def ^:dynamic *handlers*
  "Dynamically-bound list of handlers."
  '())

(s/def ::handler-key (s/nonconforming
                      (s/or :keyword qualified-keyword?
                            :class symbol?)))

(macros/deftime
(defmacro without-handlers
  "Runs the `body` in a context where no handlers are bound.
  Use with caution. It's incredibly rare that handlers should be completely
  unbound when running a given bit of code.

  The main intended usecase for this is to allow spinning up additional threads
  without the bound handlers being used. Even in this context however, most of
  the handlers which are undesirable to be run from alternate threads will be
  marked as thread-local. If the only reason to unbind handlers is to prevent
  calling a handler which may attempt to perform a non-local return, then this
  macro should not be used.

  See [[without-restarts]]."
  [& body]
  `(binding [*handlers* '()]
     ~@body)))
(s/fdef without-handlers
  :args (s/cat :body (s/* any?)))

(macros/deftime
(defmacro handler-bind
  "Runs the `body` with bound signal handlers to recover from errors.
  Each binding clause is one of the following forms:
  condition-type handler-fn
  condition-type [handler-fn & {:keys [thread-local]}]

  The condition-type must be a namespaced keyword, or a class name for the
  object used as the condition. This is tested with [[isa?]], permitting the use
  of Clojure hierarchies. Both the object and the [[type]] of it are checked
  against the condition-type.

  The handler-fn is a function of a single argument, the condition that was
  signaled.

  The thread-local configuration for a handler specifies whether or not other
  threads are allowed to invoke this handler. It defaults to false. If the
  handler performs any kind of non-local return, such as calling a restart that
  performs non-local return, signaling an error that might be handled with a
  non-local return, or calling to [[return-from]] or [[go]], it should be set to
  true.

  If the handler returns normally, then additional handlers which apply to the
  condition type are run moving up the stack, ignoring other handlers bound in
  this call, until no more are left. If all applicable handlers return normally,
  then control is returned to the signaling function."
  {:arglists '([[bindings*] exprs*])
   :style/indent 1}
  [bindings & body]
  (let [bindings (map (fn [[k f]]
                        (if-not (vector? f)
                          {::handler-fn f
                           ::condition-type k}
                          (update
                           (set/rename-keys (assoc (apply hash-map (rest f))
                                                   ::handler-fn (first f)
                                                   ::condition-type k)
                                            {:thread-local ::handler-thread})
                           ::handler-thread
                           (fn [t]
                             (macros/case
                                 :clj (when t
                                        `(Thread/currentThread))
                                 :cljs :unsupported)))))
                      (partition 2 bindings))]
    `(binding [*handlers* (conj *handlers* ~(cons 'list bindings))]
         ~@body))))
(s/fdef handler-bind
  :args (s/cat :bindings (s/and (s/* (s/cat :key ::handler-key
                                            :handler (s/or :fn-with-opts vector?
                                                           :bare-fn any?)))
                                vector?)
               :body (s/* any?)))

(defn jump-factory
  "INTERNAL: Constructs a function body which throws to the passed `target`."
  [block target]
  `(fn [& args#]
     (return-from ~block [(cons ~target args#) true])))
(s/fdef jump-factory
  :args (s/cat :block symbol?
               :target integer?))

(s/def ::handler-clause (s/cat :name (s/nonconforming
                                      (s/or :key ::handler-key
                                            :no-error #{:no-error}))
                               :arglist vector?
                               :body (s/* any?)))

(macros/deftime
(defmacro handler-case
  "Runs the `expr` with signal handlers bound, returning the value from the handler on signal.
  Bindings are of the following form:
  (condition-type [condition] & body)
  (:no-error [retvals*] & body)

  The condition-type may be a namespaced keyword or class name. The argument
  vector must have one symbol to be the condition-sym, and potentially more
  arguments based on what it is expected to be signaled with.

  If a condition handled by one of the binding clauses is signaled, the stack is
  immediately unwound out of the context of `expr`, and then the handler is run,
  with its return value used as a replacement for the return value of the entire
  `expr`.

  An additional clause which can be present is `:no-error`, which takes
  arguments for the return values of the expression (multiple may be provided
  with [[values]]), and is only run when no condition handled by this call is
  signaled.

  See [[handler-bind]]."
  {:arglists '([expr bindings*])
   :style/indent [1 :form [:defn]]}
  [expr & bindings]
  (let [bindings (map (partial s/conform ::handler-clause) bindings)
        no-error-clause (first (filter (comp #{:no-error} :name) bindings))
        bindings (filter (comp (complement #{:no-error}) :name) bindings)
        case-block (gensym "case")
        factories (map-indexed
                   (fn [idx binding]
                     [(:name binding) [(jump-factory case-block idx) :thread-local true]])
                   bindings)
        ret-val (gensym "val")
        clauses (map-indexed
                 (fn [idx binding]
                   (clojure.core/assert (= 1 (count (:arglist binding)))
                                        "Handlers take only one argument")
                   [idx `(let [~(:arglist binding) (rest ~ret-val)]
                           ~@(:body binding))])
                 bindings)]
    `(let [[~ret-val restarted?#]
           (block ~case-block
             (handler-bind [~@(mapcat identity factories)]
               [(multiple-value-list ~expr) false]))]
       (if restarted?#
         (case (long (first ~ret-val))
           ~@(mapcat identity clauses)
           (error #:farolero.control-error
                  {::condition-type ::control-error
                   :type ::invalid-clause}))
         ~(if no-error-clause
            `(let [~(:arglist no-error-clause) ~ret-val]
               ~@(:body no-error-clause))
            `(values-list ~ret-val)))))))
(s/fdef handler-case
  :args (s/and (s/cat :expr any?
                      :bindings (s/* (s/spec ::handler-clause)))
               #(<= (count (filter (comp #{:no-error} :name) (:bindings %))) 1)))

(def ^:private throwing-restart
  "A restart that throws the condition as an exception unconditionally."
  {::restart-name ::throw
   ::restart-reporter "Throw the condition as an exception"
   ::restart-interactive (constantly nil)
   ::restart-fn (fn [& args]
                  (throw (ex-info "Condition was thrown"
                                  (cond-> {}
                                    (first args) (assoc :condition (first args))
                                    (rest args) (assoc :arguments (rest args))))))})

(def ^:dynamic *restarts*
  "Dynamically-bound list of restarts."
  (list throwing-restart))

(macros/deftime
(defmacro without-restarts
  "Runs the `body` in a context where no restarts are bound.
  Use with caution. It's incredibly rare that restarts should be completely
  unbound when running a given bit of code.

  Most restarts that will be called will perform some kind of non-local return.
  In those circumstances, the restarts will already not be visible to threads
  other than the one that bound them. This means that the cases in which this
  macro are necessary are incredibly rare, and should be carefully considered.

  See [[without-handlers]]."
  [& body]
  `(binding [*restarts* (list throwing-restart)]
     ~@body)))
(s/fdef without-restarts
  :args (s/cat :body (s/* any?)))

;; NOTE(Joshua): The restart name is explicitly nilable to conform with the
;; common lisp variant which allows unnamed restarts.
(s/def ::restart-name (s/nilable qualified-keyword?))
(s/def ::restart-fn ifn?)
(s/def ::restart-test ifn?)
(s/def ::restart-interactive ifn?)
(s/def ::restart-reporter (s/or :function ifn?
                                :string string?))
;; HACK(Joshua): This is currently nilable because of the issue noted in
;; https://ask.clojure.org/index.php/11584/common-patterns-optional-fields-interact-poorly-with-spec
(s/def ::restart-thread #?(:clj (s/nilable (partial instance? Thread))
                           :cljs keyword?))
(s/def ::restart (s/keys :req [::restart-name ::restart-fn]
                         :opt [::restart-test ::restart-interactive
                               ::restart-reporter ::restart-thread]))

(macros/deftime
(defmacro restart-bind
  "Runs the `body` with bound restarts.
  Within the dynamic scope of the `body`, [[invoke-restart]] may be called with
  any of the bound restart names. This includes inside handlers bound further up
  the stack.

  Each binding clause is one of the following forms:
  restart-name restart-fn
  restart-name [restart-fn & {:keys [test-function interactive-function report-function thread-local]}]

  The restart-name is a namespaced keyword.

  The restart-fn is a function of zero or more arguments, provided by rest
  arguments on the call to [[invoke-restart]]. The function returns normally.

  The test-function is a function of an optional condition and its additional
  arguments. If it returns a truthy value, the restart is available, otherwise
  it cannot be invoked from its context. If not provided, the restart is assumed
  to be available.

  The report-function is a function or string used to display this condition to
  the user. If it is a function, it is called with the restart as an argument
  and should return a string. The restart will be a map with the key
  `:farolero.core/restart-name`. If report-function is a string, it is used
  verbatim.

  The boolean thread-local tells the system whether or not this restart may be
  invoked from other threads. It defaults to false. If the restart performs any
  kind of non-local return that cares about which thread performs it, such as a
  call to [[return-from]] or [[go]], signaling a condition which may cause a
  non-local return, or invoking a restart which may perform a non-local return,
  it should set it to true.

  The interactive-function is a function of no arguments that is called to get
  input from the user interactively. It returns a list, used as the argument
  list to restart-fn."
  {:arglists '([[bindings*] exprs*])
   :style/indent 1}
  [bindings & body]
  (let [bindings (map (fn [[k f]]
                        (if-not (vector? f)
                          {::restart-fn f
                           ::restart-name k}
                          (update
                           (set/rename-keys (assoc (apply hash-map (rest f))
                                                   ::restart-fn (first f)
                                                   ::restart-name k)
                                            {:test-function ::restart-test
                                             :interactive-function ::restart-interactive
                                             :report-function ::restart-reporter
                                             :thread-local ::restart-thread})
                           ::restart-thread
                           (fn [t]
                             (macros/case
                                 :clj (when t
                                        `(Thread/currentThread))
                                 :cljs :unsupported)))))
                      (reverse (partition 2 bindings)))]
    `(binding [*restarts* (into *restarts* ~(cons 'list bindings))]
       ~@body))))
(s/fdef restart-bind
  :args (s/cat :bindings
               (s/and (s/* (s/cat
                            :key (s/nilable keyword?)
                            :restart (s/or :fn-with-opts vector?
                                           :bare-fn any?)))
                      vector?)
               :body (s/* any?)))

(s/def ::restart-clause (s/cat :name (s/nilable keyword?)
                               :arglist vector?
                               :restart-fns (s/* (s/cat :keyword keyword?
                                                        :function any?))
                               :body (s/* any?)))

(macros/deftime
(defmacro restart-case
  "Runs the `expr` with bound restarts, returning a value from the restart on invoke.
  Bindings are of the following form:
  (restart-name [& args] config* & body)

  The restart-name is a keyword or `nil` and is used to [[find-restart]]. The
  argument vector will take any arguments passed to the restart
  via [[invoke-restart]].

  The config is a sequence of keyword-value pairs for the config options
  `:test`, `:report`, and `:interactive`, each of which corresponds to a config
  option in [[restart-bind]].

  If one of the restarts bound in this case is invoked then the stack is
  immediately unwound to outside of `expr`, and then the restart is run, with
  its return value used as a replacement for the return value of `expr`."
  {:arglists '([expr bindings*])
   :style/indent [1 :form [:defn]]}
  [expr & bindings]
  (let [bindings (map (partial s/conform ::restart-clause) bindings)
        case-block (gensym "case")
        factories (map-indexed
                   (fn [idx binding]
                     [(:name binding)
                      (apply vector (jump-factory case-block idx)
                             :thread-local true
                             (mapcat identity
                                     (set/rename-keys (into {}
                                                            (map (juxt :keyword :function)
                                                                 (:restart-fns binding)))
                                                      {:test :test-function
                                                       :report :report-function
                                                       :interactive :interactive-function})))])
                   bindings)
        ret-val (gensym "val")
        clauses (map-indexed
                 (fn [idx binding]
                   [idx `(let [~(:arglist binding) (rest ~ret-val)]
                           ~@(:body binding))])
                 bindings)]
    `(let [[~ret-val restarted?#]
           (block ~case-block
             (restart-bind [~@(mapcat identity factories)]
               [(multiple-value-list ~expr) false]))]
       (if restarted?#
         (case (long (first ~ret-val))
           ~@(mapcat identity clauses)
           (error #:farolero.control-error
                  {::condition-type ::control-error
                   :type ::invalid-clause}))
         (values-list ~ret-val))))))
(s/fdef restart-case
  :args (s/cat :expr any?
               :bindings (s/* (s/spec ::restart-clause))))

(macros/deftime
(defmacro with-simple-restart
  "Constructs a restart with the given name which unwinds and returns nil.
  Returns true as a second value with [[values]] when the restart was
  triggered.

  The `format-str` and `args` are used when reporting the restart."
  [[restart-name format-str & args] & body]
  `(restart-case (values (do ~@body) nil)
     (~restart-name []
      :report (fn [~'_] (wrap-exceptions
                          (~(macros/case
                                :clj `format
                                :cljs `goog.string/format) ~format-str ~@args)))
      (values nil true)))))
(s/fdef with-simple-restart
  :args (s/cat :restart-def (s/spec (s/cat :name (s/nilable keyword?)
                                           :format-str any?
                                           :args (s/* any?)))
               :body (s/* any?)))

(s/def ::condition (s/or :keyword (s/and keyword?
                                         namespace)
                         :other (complement keyword?)))

(defn- handles-condition?
  "Returns true if the given `handler` can handle the `condition`."
  [condition handler]
  (boolean
   (or (isa? condition handler)
       (and (map? condition)
            (isa? (::condition-type condition) handler))
       (isa? (type condition) handler))))
(s/fdef handles-condition?
  :args (s/cat :condition ::condition
               :handler (s/or :keyword keyword?
                              :class #?(:clj class?
                                        :cljs any?)))
  :ret boolean?)

(defn throwing-debugger
  "A \"debugger\" that wraps conditions with [[ex-info]] and throws them.
  If the `condition` is an [[Exception]] and no further arguments are included,
  then the `condition` is thrown directly instead."
  [condition _]
  (if (instance? #?(:clj Exception
                         :cljs js/Error)
                      condition)
    (throw condition)
    (throw (ex-info "Unhandled condition" {:condition condition
                                           :handlers (into [] (comp cat (map ::condition-type)) *handlers*)}
                    (when (instance? #?(:clj Throwable
                                        :cljs js/Error)
                                     condition)
                      condition)))))
(s/fdef throwing-debugger
  :args (s/cat :raised (s/spec (s/cat :condition ::condition
                                      :args (s/* any?)))
               :hook ifn?))

(def ^:dynamic *debugger-hook*
  "Dynamically-bound hook used in [[invoke-debugger]].
  This is a function which takes two arguments, a condition, and the currently
  bound debugger hook. This function must not return without a non-local exit."
  throwing-debugger)

(macros/case :clj
  (declare system-debugger))
(def ^:dynamic *system-debugger*
  "The debugger used when [[*debugger-hook*]] is nil.
  This happens when the error may have occurred in the debugger itself, or
  when [[break]] is called."
  (macros/case
      :clj system-debugger
      :cljs throwing-debugger))

(macros/deftime
(defmacro wrap-exceptions
  "Catches all exceptions from evaluating `body` and signals them as [[error]]s.
  This only catches [[Exception]]s, meaning [[block]], [[tagbody]], conditions,
  and restarts can all be handled through the dynamic scope of `body` without
  issue.

  If an exception is signaled as a condition, then two restarts will be bound.
  The restart `:farolero.core/continue` is bound and will retry the code which
  threw the exception and may be used if a simple retry may fix the error, or in
  cases where the handler can perform some work that will ensure the operation
  succeeds. The restart `:farolero.core/use-value` takes one argument and will
  return it without modification as a replacement for the value returned by the
  macro call.

  If the `:farolero.core/use-value` restart is invoked interactively it will
  signal `:farolero.core/interactive-wrap-exceptions` with the exception as an
  argument, with an additional `:farolero.core/use-value` restart bound to
  provide the value to use for the outer restart."
  {:style/indent 0}
  [& body]
  `(block outer-block#
     (tagbody
      eval#
      (try (return-from outer-block#
             (do ~@body))
           (catch ~(macros/case :clj 'java.lang.Exception :cljs 'js/Error) e#
             (return-from
                 outer-block#
               (restart-case (error e#)
                 (::continue []
                   :report "Ignore the exception and retry evaluation"
                   (go eval#))
                 (::use-value [v#]
                   :report "Ignore the exception and use the passed value"
                   :interactive (fn []
                                  (list
                                   (request-value
                                    [::interactive-wrap-exceptions e#]
                                    "Enter a value to use in place of the exception")))
                   v#)))))))))
(s/fdef wrap-exceptions
  :args (s/cat :body (s/* any?)))

(defn- ->condition
  ([simple condition]
   (cond
     (map? condition) condition
     (keyword? condition) {::condition-type condition}
     (string? condition) {::condition-type simple
                          :farolero.simple-condition/format-string condition}
     (isa? (type condition) ::condition) {::condition-type (type condition)
                                          ::condition-value condition}

     :else
     (error #:farolero.control-error
            {::condition-type ::control-error
             :type ::invalid-condition
             :farolero.control-error.invalid-condition/condition-value condition})))
  ([simple condition args]
   (if (string? condition)
     #:farolero.simple-condition
     {::condition-type simple
      :format-string condition
      :format-arguments args}
     (let [[& {:as argmap}] args]
       (cond
         (map? condition)
         (merge condition argmap)

         (keyword? condition)
         (merge {::condition-type condition} argmap)

         (isa? (type condition) ::condition)
         (assoc argmap
                ::condition-type (type condition)
                ::condition-value condition)

         :else
         (error #:farolero.control-error.invalid-condition
                {::condition-type ::control-error
                 :farolero.control-error/type ::invalid-condition
                 :condition-value condition
                 :condition-arguments args}))))))

(defn invoke-debugger
  "Calls the [[*debugger-hook*]], or [[*system-debugger*]] if not bound, with the `condition`."
  [condition & args]
  (let [condition (->condition ::simple-error condition args)]
    (if *debugger-hook*
      (let [hook *debugger-hook*]
        (binding [*debugger-hook* nil]
          (hook condition hook)))
      (*system-debugger* condition *system-debugger*))))
(s/fdef invoke-debugger
  :args (s/cat :condition ::condition))

(defn break
  "Binds the system debugger and invokes it on the given `condition`.
  Binds the restart `:farolero.core/continue` during debugging which will exit
  the debugger normally."
  [condition]
  (binding [*debugger-hook* nil]
    (let [condition (if (string? condition)
                      {::condition-type ::simple-condition
                       :farolero.simple-condition/message condition}
                      condition)]
      (restart-case (invoke-debugger condition)
        (::continue [] :report "Continue out of the debugger")))))
(s/fdef break
  :args (s/cat :condition ::condition
               :args (s/* any?)))

(def ^:dynamic *break-on-signals*
  "Dynamically-bound type of signal to [[break]] on.
  If this is non-nil, then any condition which matches it with
  [[handles-condition?]] which is signaled will [[break]]. If it is `true` then
  this will occur for all conditions."
  nil)

(derive ::simple-condition ::condition)

(defn- ensure-derived
  "Ensures that `child` derives from `parent`.

  If `child` is a keyword, derives directly. If it is not, derives the [[type]]
  of `child` from `parent`."
  [child parent]
  (let [child-type (if (keyword? child)
                     child
                     (type child))]
    (when-not (or (contains? (ancestors child-type) parent)
                  (= child-type parent))
      (derive child-type parent))))

(defn signal
  "Signals a `condition`, triggering handlers bound for the condition type.
  Looks up the stack for handlers which apply to the given `condition` and then
  applies them in sequence until they all complete or one calls
  [[invoke-restart]]. If this function returns normally, it will return nil."
  [condition & args]
  (let [condition (->condition ::simple-condition condition args)]
    (ensure-derived (::condition-type condition) ::condition)
    (when (or (true? *break-on-signals*)
              (handles-condition? condition *break-on-signals*))
      (break (str "Breaking on signal " (pr-str condition))))
    (loop [remaining-clusters *handlers*]
      (when (seq remaining-clusters)
        (binding [*handlers* (rest remaining-clusters)]
          (let [cluster (first remaining-clusters)]
            (doseq [{::keys [condition-type handler-fn handler-thread]} cluster
                    :when (handles-condition? condition condition-type)]
              (when (or (= handler-thread #?(:clj (Thread/currentThread)
                                             :cljs :unsupported))
                        (not handler-thread))
                (handler-fn condition)))))
        (recur (rest remaining-clusters)))))
  nil)
(s/fdef signal
  :args (s/cat :condition ::condition
               :args (s/* any?))
  :ret nil?)

(derive ::interaction ::condition)
(derive ::request-value ::interaction)

(defn request-value
  "Request a value from the user interactively.

  Signals `condition` with a `:farolero.core/use-value` restart bound to return
  the passed value. If the signal is not handled, `prompt` is printed
  to [[*out*]] and a repl prompt is printed. The user can enter an expression
  that evaluates to the value to use.

  The first argument to the condition with be `prompt`, followed by `valid?`. If
  no function `valid?` is provided, [[any?]] is passed instead.

  If `condition` is [[sequential?]] then the first element is signaled as a
  condition with the rest as additional arguments after `prompt` and `valid?`.
  If you wish to signal a [[sequential?]] argument, you must wrap it in an
  additional sequence.

  If the `condition` which gets signaled does not already [[derive]] from
  `:farolero.core/request-value`, it will be made to do so.

  See [[request-interaction]]."
  {:arglists '([condition] [prompt] [condition prompt] [prompt valid?] [condition prompt valid?])}
  ([condition-or-prompt]
   (if (string? condition-or-prompt)
     (request-value ::request-value condition-or-prompt nil)
     (request-value condition-or-prompt nil nil)))
  ([condition-or-prompt prompt-or-valid?]
   (if (string? condition-or-prompt)
     (request-value ::request-value condition-or-prompt prompt-or-valid?)
     (request-value condition-or-prompt prompt-or-valid? nil)))
  (#_{:clj-kondo/ignore #?(:clj [] :cljs [:unused-binding])}
   [condition-type prompt valid?]
   (restart-case
       #_{:clj-kondo/ignore #?(:clj [] :cljs [:redundant-do])}
       (let [condition {::condition-type condition-type
                        ::prompt prompt
                        ::valid? (or valid? any?)}]
         (ensure-derived (::condition-type condition) ::request-value)
         (signal condition)
         #?@(:clj ((when prompt
                     (println prompt))
                   (block complete
                     (tagbody
                      retry
                      (let [v (restart-case
                                  (wrap-exceptions
                                    (print (str (ns-name *ns*) "> "))
                                    (flush)
                                    (doto (eval (read))
                                      prn))
                                (::abort [] :report "Abort this evaluation and retry"
                                  (go retry)))]
                        (if (or (not valid?)
                                (valid? v))
                          (return-from complete v)
                          (do (println "Invalid value, please try again")
                              (go retry)))))))))
     (::use-value [v]
       :report "Use the passed value as the argument to the interactive restart"
       (if (valid? v)
         v
         (recur condition-type prompt valid?))))))

(derive ::request-interaction ::interaction)

(defn request-interaction
  "Requests the user perform some interaction before the program continues.

  Signals `condition` with a `:farolero.core/continue` restart bound to continue
  execution. If the signal is not handled, `prompt` is printed to [[*out*]] and
  a repl prompt is printed, along with instruction to call [[complete]] when the
  user is done interacting with the system.

  The first argument to the condition will be the `prompt`.

  If `condition` is [[sequential?]] then the first element is signaled as a
  condition with the rest as further arguments after the `prompt`. If you wish
  to signal a [[sequential?]] argument, you must wrap it in an additional
  sequence.

  If the `condition` which gets signaled does not already [[derive]] from
  `:farolero.core/request-interaction`, it will be made to do so.

  See [[request-value]]."
  {:arglists '([condition] [prompt] [condition prompt])}
  ([condition-or-prompt]
   (if (string? condition-or-prompt)
     (request-interaction ::request-interaction condition-or-prompt)
     (request-interaction condition-or-prompt nil)))
  (#_{:clj-kondo/ignore #?(:clj [] :cljs [:unused-binding])}
   [condition-type prompt]
   (restart-case
       (let [condition {::condition-type condition-type
                        ::prompt prompt}]
         (ensure-derived (::condition-type condition) ::request-interaction)
         (signal condition)
         #?@(:clj ((when prompt
                     (println prompt))
                   (println "Call farolero.core/continue when you are done")
                   (loop []
                     (wrap-exceptions
                       (print (str (ns-name *ns*) "> "))
                       (flush)
                       (prn (eval (read))))
                     (recur)))))
     (::continue [] :report "Complete the interaction request and continue"))))

(defn report-restart
  "Reports the restart using the its report-function."
  [{:as restart ::keys [restart-name restart-reporter]}]
  (cond
    (not restart-reporter) restart-name
    (string? restart-reporter) restart-reporter
    (ifn? restart-reporter) (wrap-exceptions
                              (restart-reporter restart))))
(s/fdef report-restart
  :args (s/cat :restart ::restart))

(defmulti report-condition
  "Multimethod for creating a human-readable explanation of a condition.
  Takes a `condition` and `args` and returns a string describing them.

  The dispatch value is a condition-type as in [[handler-bind]]."
  (fn [condition]
    (cond
      (keyword? condition) condition
      (map? condition) (::condition-type condition)
      :else (type condition))))
(s/fdef report-condition
  :args (s/cat :condition ::condition))

(defmethod report-condition :default
  [condition]
  (str (pr-str (if (keyword? condition)
                 condition
                 (type condition)))
       " was signaled"))

(macros/case
    :clj
    #_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-symbol])}
    (defmethod report-condition Exception
      [condition]
      (if-some [msg (ex-message condition)]
        msg
        (pr-str (type condition))))
    :cljs
    #_{:clj-kondo/ignore #?(:clj [:unresolved-namespace] :cljs [])}
    (defmethod report-condition js/Error
      [condition]
      (if-some [msg (.-message condition)]
        msg
        (pr-str (type condition)))))

(macros/usetime
(defmethod report-condition ::simple-condition
  [{:farolero.simple-condition/keys [message format-string format-arguments]}]
  (cond
    message message
    format-string (wrap-exceptions (apply #?(:clj format :cljs gstring/format) format-string format-arguments))
    :else "A simple condition")))

(defmethod report-condition ::type-error
  [{:farolero.type-error/keys [description value spec]}]
  (str "The value doesn't conform to spec " description
       "\nSpec:" (pr-str spec)
       "\nValue:" (pr-str value)))

(derive ::warning ::condition)
(derive ::simple-warning ::warning)
(derive ::simple-warning ::simple-condition)

(def ^:dynamic *warning-printer*
  "Dynamically-bound function used to display a warning to the user.

  This must be a function taking a `condition`. The function
  [[report-condition]] may be used to assist in generating the error string.

  The default value will write the condition to [[*err*]], including any stack
  trace on the condition if it is an exception type."
  (fn [condition]
    (binding #?(:clj [*out* *err*]
                :cljs [*print-fn* *print-err-fn*])
      (println "WARNING:" (report-condition condition))
      (when (instance? #?(:clj Throwable
                          :cljs js/Error)
                       condition)
        #?(:clj (st/print-cause-trace condition)
           :cljs (pr (.stack condition)))))))

(defn warn
  "Signals a condition, reporting a warning if not handled.
  Returns nil. Reports the warning using [[*warning-printer*]].

  Binds a restart called `:farolero.core/muffle-warning`, which prevents the
  warning without any additional side effects. This restart may be invoked
  directly by calling [[muffle-warning]].

  The `condition` will be modified to derive from `:farolero.core/warning`. If
  it is a keyword, it will derive directly, otherwise it will derive
  the [[type]].

  See [[signal]]."
  [condition & args]
  (let [condition (->condition ::simple-warning condition args)]
    (ensure-derived (::condition-type condition) ::warning)
    (restart-case (do (signal condition)
                      (*warning-printer* condition))
      (::muffle-warning [] :report "Ignore the warning and continue")))
  nil)
(s/fdef warn
  :args (s/cat :condition ::condition
               :args (s/* any?))
  :ret nil?)

(derive ::error ::condition)
(derive ::simple-error ::error)
(derive ::simple-error ::simple-condition)
(macros/case
    :clj
    #_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-symbol])}
    (derive Exception ::error)
    :cljs
    #_{:clj-kondo/ignore #?(:clj [:unresolved-namespace] :cljs [])}
    (derive js/Error ::error))

(defn error
  "Signals a condition, calling [[invoke-debugger]] if any handlers return normally.

  See [[signal]]."
  [condition & args]
  (let [condition (->condition ::simple-error condition args)]
    (ensure-derived (::condition-type condition) ::error)
    (signal condition)
    (invoke-debugger condition)))
(s/fdef error
  :args (s/cat :condition ::condition
               :args (s/* any?))
  :ret nil?)

(defn cerror
  "Signals a condition as [[error]], but binds a restart to continue.
  The `:farolero.core/continue` restart is bound for any handlers invoked by
  this error. This restart may be invoked directly by calling [[continue]].

  The `report-fmt` is used as the argument to `:report` in the resulting
  restart.

  See [[signal]]."
  ([] (cerror "Ignore the error and continue"))
  ([report-fmt] (cerror report-fmt "An error has occurred"))
  ([report-fmt condition & args]
   (restart-case (apply error condition args)
     (::continue [] :report report-fmt))))
(s/fdef cerror
  :args (s/cat :report-fmt (s/? (s/or :function ifn?
                                      :string string?))
               :condition (s/? any?)
               :args (s/* any?))
  :ret nil?)

(defn compute-restarts
  "Returns a sequence of all usable restarts.
  Any restart with a `:farolero.core/restart-test` function will be filtered
  based on if it returns a truthy value when called with `condition`."
  ([] (compute-restarts nil))
  ([condition]
   (filter #(and (or (= (::restart-thread %) #?(:clj (Thread/currentThread)
                                                :cljs :unsupported))
                     (not (::restart-thread %)))
                 (wrap-exceptions
                   ((::restart-test % (constantly true)) condition)))
           *restarts*)))
(s/fdef compute-restarts
  :args (s/cat :condition (s/? any?))
  :ret (s/coll-of ::restart))

(defn find-restart
  "Returns the first restart bound named by `restart-name`."
  ([restart-name] (find-restart restart-name nil))
  ([restart-name condition]
   (first (filter (comp #{restart-name} ::restart-name) (compute-restarts condition)))))
(s/fdef find-restart
  :args (s/cat :restart-name ::restart-name
               :condition (s/? any?))
  :ret ::restart)

(defn invoke-restart
  "Calls a restart by the given name with `args`.
  If the restart isn't found, signals a `:farolero.core/control-error`.

  Throws an assertion exception if called outside a restart context.

  See [[restart-bind]], [[restart-case]]."
  [restart-name & args]
  (if-let [restart (if (keyword? restart-name)
                     (find-restart restart-name)
                     restart-name)]
    (apply (::restart-fn restart) args)
    (error #:farolero.control-error
           {::condition-type ::control-error
            :type ::missing-restart
            :restart-name restart-name
            :available-restarts (compute-restarts)})))
(s/fdef invoke-restart
  :args (s/cat :restart-name (s/or :name keyword?
                                   :restart ::restart)
               :args (s/* any?)))

(def ^:private interactive-lock
  "An object used to ensure interactive restarts are invoked serially."
  #?(:clj (Object.)
     :cljs nil))

(defn invoke-restart-interactively
  "Calls a restart by the given name interactively.
  If the restart was created with an `:interactive-function`, then it is called
  to produce the argument list for the restart. Otherwise, a default is used. In
  Clojure, the default is to read and evaluate from [[*in*]]. In ClojureScript,
  the default is to produce nil as the arguments.

  See [[invoke-restart]]"
  [restart-name]
  (locking interactive-lock
    (if-let [restart (if (keyword? restart-name)
                       (find-restart restart-name)
                       restart-name)]
      (apply invoke-restart restart-name
             ((or (::restart-interactive restart)
                  (constantly nil))))
      (error #:farolero.control-error
             {::condition-type ::control-error
              :type ::missing-restart
              :restart-name restart-name
              :available-restarts (compute-restarts)}))))
(s/fdef invoke-restart-interactively
  :args (s/cat :restart-name (s/or :name keyword?
                                   :restart ::restart)))

(defn muffle-warning
  "Ignores the warning and continues.
  Invokes the `:farolero.core/muffle-warning` restart.

  See [[warn]]."
  ([] (muffle-warning nil))
  ([condition]
   (invoke-restart (find-restart ::muffle-warning condition))))
(s/fdef muffle-warning
  :args (s/cat :condition (s/? any?)))

(defn continue
  "Ignores the signaled condition and continues.
  Invokes the `:farolero.core/continue` restart.
  If the restart isn't present, returns nil.

  See [[cerror]]."
  ([] (continue nil))
  ([condition]
   (when-let [restart (find-restart ::continue condition)]
     (invoke-restart restart))))
(s/fdef continue
  :args (s/cat :condition (s/? any?)
               :args (s/* any?)))

(defn use-value
  "Uses `val` instead of the value which caused the error.
  Invokes the `:farolero.core/use-value` restart.
  If the restart isn't present, returns nil."
  ([val] (use-value val nil))
  ([val condition]
   (when-let [restart (find-restart ::use-value condition)]
     (invoke-restart restart val))))
(s/fdef use-value
  :args (s/cat :val any?
               :condition
               (s/? (s/cat :condition any?))))

(defn store-value
  "Stores the `val` in a way determined by the restart.
  Invokes the `:farolero.core/store-value` restart.
  If the restart isn't present, returns nil.

  See [[store-value-fn]]."
  ([val] (store-value val nil))
  ([val condition]
   (when-let [restart (find-restart ::store-value condition)]
     (invoke-restart restart val))))
(s/fdef store-value
  :args (s/cat :val any?
               :condition (s/? any?)))

(defn store-value-fn
  "Stores the `val` using `store-fn`.
  Invokes the `:farolero.core/store-value` restart.
  `store-fn` is used as a method to store values in the place. This may be used
  to provide [[clojure.core/swap!]], [[clojure.core/vswap!]], or other methods
  of storing values in a mutable storage.

  See [[store-value]]."
  ([store-fn val] (store-value-fn store-fn val nil))
  ([store-fn val condition]
   (when-let [restart (find-restart ::store-value condition)]
     (invoke-restart restart store-fn val))))
(s/fdef store-value-fn
  :args (s/cat :store-fn ifn?
               :val any?
               :condition (s/? any?)
               :args (s/* any?)))

(defn abort
  "Aborts the current computation.
  Invokes the `:farolero.core/abort` restart."
  ([] (abort nil))
  ([condition]
   (invoke-restart (find-restart ::abort condition))))
(s/fdef abort
  :args (s/cat :condition (s/? any?)
               :args (s/* any?)))

(macros/deftime
(defmacro ignore-errors
  "Evaluates the `body`, returning nil if any errors are signaled.
  Any arguments passed to the restart are returned as additional [[values]]."
  {:style/indent 0}
  [& body]
  `(handler-case (do ~@body)
     (::error [condition#] (values nil condition#)))))
(s/fdef ignore-errors
  :args (s/cat :body (s/* any?)))

(defmulti report-control-error
  "Multimethod for creating a human-readable explanation of a control error.
  Dispatches on the `:type` key of the error."
  (fn [error]
    (:type error)))

(defmethod report-control-error :default
  [{:keys [description] :as error}]
  (str description (when description "\n")
       (pr-str (dissoc error :description))))

(defmethod report-control-error ::missing-restart
  [{:keys [restart-name available-restarts]}]
  (str "Missing the restart "
       (pr-str restart-name)
       "\nAvailable restarts "
       (pr-str (map ::restart-name available-restarts))))

(defmethod report-control-error ::outside-block
  [{:keys [block-name]}]
  (str "Cannot return from a block (" block-name ") outside its dynamic scope"))

(defmethod report-control-error ::invalid-clause
  [_]
  (str "Attempted to jump to an invalid clause; something's"
       " gone wrong with farolero, please report this to the maintainer"))

(defmethod report-condition ::control-error
  [_ & {:as opts}]
  (wrap-exceptions
    (report-control-error opts)))

(def ^:dynamic *place*
  "The place being modified in an interactive continue from [[assert]]."
  nil)

(derive ::assertion-error ::error)
(derive ::interactive-assertion ::request-interaction)

(macros/deftime
(defmacro assert
  "Evaluates `test` and raises `condition` if it does not evaluate truthy.
  The restart `:farolero.core/continue` is bound when the condition is raised to
  retry the computation, and when invoked interactively prompts the user for new
  values for each of the provided `places`, during which `:farolero.core/abort`
  is bound to retry in the case of a failed assignment.

  The interactive function is extensible by handling conditions of type
  `:farolero.core/interactive-assertion` with a single extra argument, a list of
  tuples of the values of the `places` and the forms that evaluate to them.
  During this a `:farolero.core/continue` restart is bound to skip requesting
  input from the user."
  ([test]
   `(assert ~test []))
  ([test places]
   `(assert ~test ~places ::assertion-error))
  ([test places condition & args]
   (let [places-sym (gensym)]
     `(tagbody
       retry#
       (restart-case (when-not ~test
                       (error ~condition ~@args))
         (::continue []
           :interactive
           ;; FIXME(Joshua): This produces a filename that's too large because
           ;; of all the nested functions. Need to figure out how to fix this if
           ;; possible.
           (fn []
             (restart-case
                 (let [~places-sym ~(cons 'list (map vector places (map (partial list 'quote) places)))]
                   (signal #:farolero.interactive-assertion
                           {::condition-type ::interactive-assertion
                            :places ~places-sym})
                   ~(macros/case :clj
                      `(doseq [[place# form#] ~places-sym]
                         (println (str "The old value of " (pr-str form#) " is " (pr-str @place#)))
                         (print "Provide a new value? (y or n) ")
                         (flush)
                         ;; FIXME(Joshua): Really shouldn't need to read a line
                         ;; here as far as I can tell, but this is required to
                         ;; make it work whenever I've tested it.
                         (read-line)
                         (flush)
                         (when (= \y (first (read-line)))
                           (with-simple-restart (::continue "Continue without providing a new value")
                             (block return#
                               (tagbody
                                loop#
                                (println "Provide an expression to change the value of" form#)
                                (print (str (ns-name *ns*) "> "))
                                (flush)
                                (multiple-value-bind
                                    [[val# restarted?#]
                                     (with-simple-restart (::abort "Abort this read and retry")
                                       (binding [*place* place#]
                                         (wrap-exceptions
                                           (prn (eval (read))))))]
                                  (if restarted?#
                                    (go loop#)
                                    (return-from return# val#))))))))))
               (::continue []))
             nil)
           :report "Retry the assertion, setting new values interactively"
           (go retry#))))))))
(s/fdef assert
  :args (s/cat :test any?
               :places (s/? (s/coll-of any? :kind vector?))
               :condition (s/? any?)
               :args (s/* any?)))

(derive ::type-error ::error)
(derive ::interactive-check-type ::request-value)

(macros/deftime
(defmacro check-type
  "Checks to see if the value stored in `place` conforms to `spec`.
  `place` must evaluate to an implementation of IDeref. Raises a
  `:farolero.core/type-error` if it does not conform. Binds a
  `:farolero.core/store-value` restart taking a function to modify `place` and a
  value to use as its second argument. Also binds `:farolero.core/continue` to
  retry check.

  If the `:farolero.core/store-value` restart is invoked interactively, it will
  signal `:farolero.core/interactive-check-type`, passing the form for `place`,
  binding a further `:farolero.core/store-value` restart which expects the
  modify function and argument for passing to the outer restart."
  ([place spec]
   `(check-type ~place ~spec nil))
  ([place spec type-description]
   (let [form (gensym "form")]
     `(let [place# ~place
            ~form (quote ~place)
            spec# ~spec]
        (tagbody
         retry-check#
         (restart-case (let [value# (wrap-exceptions
                                      @place#)]
                         (when-not (s/valid? spec# value#)
                           (error #:farolero.type-error
                                  {::condition-type ::type-error
                                   :description ~type-description
                                   :value value#
                                   :spec spec#
                                   :result (s/explain-data spec# value#)}))
                         (go exit#))
           (::store-value [modify-fn# new-val#]
             :interactive (fn []
                            (restart-case
                                (do
                                  (signal {::condition-type ::interactive-check-type
                                           :farolero.check-type/form ~form})
                                  ~@(macros/case :clj
                                      `((println "Provide a new value for " (pr-str ~form))
                                        [(loop []
                                           (print "Provide a function to modify the place (e.g. clojure.core/swap!): ")
                                           (flush)
                                           (let [sym# (wrap-exceptions
                                                        (read))]
                                             (if-let [fn# (and (symbol? sym#)
                                                               (resolve sym#))]
                                               fn#
                                               (recur))))
                                         (block return#
                                           (tagbody
                                            loop#
                                            (println "Provide a value for the second argument of the function:")
                                            (print (str (ns-name *ns*) "> "))
                                            (flush)
                                            (multiple-value-bind [[val# restarted?#]
                                                                  (with-simple-restart (::abort "Abort this evaluation and retry")
                                                                    (wrap-exceptions
                                                                      (eval (read))))]
                                              (if restarted?#
                                                (go loop#)
                                                (return-from return# val#)))))])
                                      :cljs
                                      `(nil)))
                              (::store-value [fn# v#]
                                (list fn# v#))
                              (::use-value [v#] v#)))
             :report "Stores the value using the provided function"
             (with-simple-restart (::abort "Abort setting a new value")
               (wrap-exceptions
                 (modify-fn# place# new-val#)))
             (go retry-check#)))
         exit#))))))
(s/fdef check-type
  :args (s/cat :place any?
               :spec any?
               :type-description (s/? (s/nilable string?))))

#_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-symbol :unresolved-namespace])}
(macros/case :clj
(do
(defmacro ^:private with-abort-restart
  "Evaluates the `body` with an `:farolero.core/abort` restart bound."
  {:style/indent 0}
  [& body]
  `(let [level# (get *debugger-level* (Thread/currentThread) 0)]
     (with-simple-restart (::abort (str "Return to level " level# " of the debugger"))
       ~@body)))
(s/fdef with-abort-restart
  :args (s/cat :body (s/* any?)))

(def ^:dynamic ^:private *debugger-level*
  "Dynamic variable containing the current level of the system debugger."
  {})

(def ^:dynamic *debugger-condition*
  "Dynamic variable with the condition currently signaled in the debugger."
  nil)

(def debugger-wait-queue
  "A map of threads to the condition they are waiting on."
  (ref {}))
(def debugger-thread
  "The current thread that the debugger is active on, if any."
  (ref nil))

(defn- acquire-debugger
  "Sets this thread to the active debugger thread, awaiting to be notified if already in use.
  Locks the debugger (the value stored in [[debugger-wait-queue]]) for the body,
  waiting on it if the debugger is in use. When it completes it will notify on
  the debugger to coordinate thread handoff between debuggers."
  [^Object debugger]
  (locking debugger
    (tagbody
     (when (dosync
            (when @debugger-thread
              (alter debugger-wait-queue assoc (Thread/currentThread) debugger)
              true))
       (.wait debugger))
     (go try-exit)

     wait-for-debugger
     (when @debugger-thread
       (.wait debugger))

     try-exit
     (dosync
      (when @debugger-thread
        (go wait-for-debugger))
      (ref-set debugger-thread (Thread/currentThread))
      (alter debugger-wait-queue dissoc (Thread/currentThread)))
     (.notify debugger))))

(defn- release-debugger
  "Releases the debugger from the current thread and activates another.
  Notifies the released debugger to wake its thread and waits on it accepting it
  before returning to prevent this thread from hogging the debugger."
  ([]
   (release-debugger (first (vals @debugger-wait-queue))))
  ([^Object debugger]
   (dosync
    (when-not @debugger-thread
      (warn "Debugger was released without a thread bound"))
    (ref-set debugger-thread nil))
   (when debugger
     (locking debugger
       (.notify debugger)
       (.wait debugger)))))

(defmethod report-control-error ::invalid-debugger
  [_]
  (str "Attempted to invoke an invalid debugger"))

(defn- switch-debugger
  "Gets user input to change which debugger is active.
  Signals a control error if an invalid debugger id is passed."
  []
  (tagbody
   loop
   (let [debuggers (seq @debugger-wait-queue)]
     (println "Debuggers from other threads")
     (dorun
      (map-indexed
       (fn [idx [thread debugger]]
         (println (str idx " [" (.getName ^Thread thread) "] " (report-condition debugger))))
       debuggers))
     (print "Debugger to activate: ")
     (flush)
     (restart-bind [::continue [#(go loop)
                                :report-function "Retry reading a debugger index and continue"]]
       (let [v (read)]
         (if (and (nat-int? v)
                  (< v (count debuggers)))
           (release-debugger (second (nth debuggers v)))
           (error #:farolero.control-error
                  {::condition-type ::control-error
                   :type ::invalid-debugger})))))))

(defn system-debugger
  "Recursive debugger used as the default.
  Binds [[*debugger-level*]], and [[*debugger-condition*]] inside the debugger.

  Reports the restarts available for the given condition, and provides a repl to
  evaluate arbitrary values to prepare for invoking them. Entering a number at
  the repl will invoke the corresponding restart interactively.

  If another error is signaled without being handled, an additional layer of
  the debugger is invoked."
  [condition _]
  (let [debugger condition]
    (try
      (tagbody
       re-acquire-debugger
       (when (zero? (get *debugger-level* (Thread/currentThread) 0))
         (acquire-debugger debugger))
       (binding [*debugger-hook* nil
                 *system-debugger* system-debugger
                 *debugger-level* (update *debugger-level* (Thread/currentThread) (fnil inc 0))
                 *debugger-condition* condition]
         (tagbody
          print-banner
          (println (str "Debugger level " (get *debugger-level* (Thread/currentThread) 0) " entered on "
                        (if (keyword? condition)
                          condition
                          (type condition))
                        "\n"
                        (report-condition condition)))
          (let [restarts (compute-restarts condition)]
            (dorun
             (map-indexed (fn [idx restart]
                            (println (str idx " [" (::restart-name restart) "]"
                                          " " (report-restart restart))))
                          restarts))
            (let [prompt #(do (print (str (ns-name *ns*) "> "))
                              (flush))
                  _ (prompt)
                  restart
                  (loop [form (read)]
                    (cond
                      (and (number? form)
                           (< form (count restarts)))
                      form

                      (= form :switch-debugger)
                      (do (with-abort-restart
                            (switch-debugger)
                            (go re-acquire-debugger))
                          (go print-banner))

                      :else
                      (do (multiple-value-bind
                              [[_ restarted?]
                               (with-abort-restart
                                 (wrap-exceptions
                                   (prn (eval form))))]
                            (when restarted?
                              (go print-banner)))
                          (prompt)
                          (recur (read)))))]
              (with-abort-restart
                (invoke-restart-interactively (nth restarts restart))
                (go print-banner))
              (go print-banner))))))
      (finally
        (when (zero? (get *debugger-level* (Thread/currentThread) 0))
          (release-debugger))))))
(s/fdef system-debugger
  :args (s/cat :raised (s/spec (s/cat :condition ::condition))
               :hook ifn?))

(alter-var-root #'*system-debugger* (constantly system-debugger))))
