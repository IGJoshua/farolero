(ns farolero.core
  "Common Lisp style handlers and restarts for errors."
  (:require
   [farolero.proto :as p :refer [Jump args is-target?]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   #?@(:clj ([net.cgrand.macrovich :as macros]
             [clojure.stacktrace :as st])
       :cljs ([farolero.signal :refer [->Signal]])))
  #?(:clj
     (:import
      [farolero.signal Signal])
     :cljs
     (:require-macros
      [net.cgrand.macrovich :as macros]))
  (:refer-clojure :exclude [assert]))

(declare error)

(def ^:dynamic *bound-blocks*
  "A set of blocks that the code is currently in the dynamic scope of."
  #{})

(defn block*
  "Calls `f`, so that it may be escaped by calling [[return-from]], passing `block-name`.
  This is analogous to Common Lisp's `catch` operator, with [[return-from]]
  being passed a keyword directly replacing `throw`."
  {:style/indent [:defn]}
  [block-name f & args]
  (try (binding [*bound-blocks* (conj *bound-blocks* block-name)]
         (apply f args))
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

(defn make-jump-target
  "INTERNAL: Constructs a new [[gensym]]med keyword used as the target of a jump."
  []
  (keyword "farolero.core" (name (gensym "jump-target"))))
(s/fdef make-jump-target
  :ret keyword?)

(defmacro block
  "Constructs a named block which can be escaped by [[return-from]]."
  {:style/indent [:defn]}
  [block-name & body]
  (if (keyword? block-name)
    `(block* ~block-name
       (fn [] ~@body))
    `(let [~block-name (make-jump-target)]
       (block* ~block-name
         (fn [] ~@body)))))
(s/fdef block
  :args (s/cat :block-name (s/or :lexical symbol?
                                 :dynamic keyword?)
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

(defn return-from
  "Performs an early return from a named [[block]]."
  ([block-name] (return-from block-name nil))
  ([block-name value]
   (when-not (contains? *bound-blocks* block-name)
     (error ::control-error "Cannot return from a block outside its dynamic scope"))
   (throw (make-jump block-name (list value)))))
(s/fdef return-from
  :args (s/cat :block-name keyword?
               :value (s/? any?)))

(def ^:dynamic *in-tagbodies* #{})

(s/def ::tagbody-args (s/cat :initial-expr (s/* (comp not symbol?))
                             :clauses (s/* (s/cat :clause-tag symbol?
                                                  :clause-body (s/* (comp not symbol?))))))

(defmacro tagbody
  "Performs the clauses in order, returning nil, allowing [[go]] between clauses.
  Each clause is in the following form:
  tag forms*

  The tag is a symbol naming the clause. Optionally any number of forms may be
  placed before the first tag, and these will execute first, although there is
  no way to jump to them after their execution."
  [& clauses]
  (let [clauses (s/conform ::tagbody-args clauses)
        init (:initial-expr clauses)
        clauses (:clauses clauses)
        tags (map :clause-tag clauses)
        target (make-jump-target)
        go-targets (map-indexed (fn [idx tag]
                                  {:jump-target target
                                   :clause-index idx})
                                tags)
        clauses (map-indexed (fn [idx clause]
                               [idx
                                `(do ~@(:clause-body clause)
                                     ~(inc idx))])
                             clauses)
        e (gensym)]
    `(block tagbody#
       (let [[~@tags] ~(cons 'list go-targets)]
         (binding [*in-tagbodies* (conj *in-tagbodies* ~target)]
           (loop [control-pointer# nil]
             (let [next-ptr#
                   (block ~target
                     (case control-pointer#
                          nil (do ~@init
                                  0)
                          ~@(mapcat identity clauses)
                          (return-from tagbody#)))]
               (recur next-ptr#))))))))
(s/fdef tagbody
  :args ::tagbody-args)

(defn go
  "Jumps to the given `tag` in the surrounding [[tagbody]]."
  [tag]
  (when-not (contains? *in-tagbodies* (:jump-target tag))
    (error ::control-error "you can only use go inside the dynamic scope of its matching tagbody"))
  (return-from (:jump-target tag) (:clause-index tag)))

(def ^:dynamic *extra-values*
  "Dynamic variable for returning multiple values up the stack."
  nil)

(defmacro multiple-value-bind
  "Binds multiple return values.
  Additional return values can only be provided by [[values]]."
  {:style/indent [:defn]}
  [[binding expr] & body]
  `(binding [*extra-values* ::empty-binding]
     (let [~binding (cons ~expr *extra-values*)]
       ~@body)))

(defmacro values
  "Returns multiple values.
  The first value is the \"true\" return value. Additional values may be bound
  using [[multiple-value-bind]]."
  [value & more]
  `(let [ret# ~value]
     (when (= *extra-values* ::empty-binding)
       (set! *extra-values* ~(cons 'list more)))
     ret#))

(def ^:dynamic *handlers*
  "Dynamically-bound list of handlers."
  '())
(def ^:dynamic *in-restartable-context*
  "Dynamically-bound boolean stating whether or not restarts are allowed."
  false)

(s/def ::handler-key (s/nonconforming
                      (s/or :keyword keyword?
                            :class symbol?)))

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
  (let [bindings (map vec (partition 2 bindings))]
    `(binding [*handlers* (conj *handlers* [(macros/case
                                                :clj (Thread/currentThread)
                                                :cljs :unsupported)
                                            ~(cons 'list bindings)])]
       ~@body)))
(s/fdef handler-bind
  :args (s/cat :bindings (s/and (s/* (s/cat :key ::handler-key
                                            :handler any?))
                                vector?)
               :body (s/* any?)))

(defn jump-factory
  "INTERNAL: Constructs a function body which throws to the passed `target`."
  [block target]
  `(fn [& args#]
     (return-from ~block (cons ~target args#))))
(s/fdef jump-factory
  :args (s/cat :block symbol?
               :target keyword?))

(s/def ::handler-clause (s/cat :name ::handler-key
                               :arglist vector?
                               :body (s/* any?)))

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
  (let [bindings (map (partial s/conform ::handler-clause) bindings)
        case-block (gensym)
        targets (repeatedly (count bindings) make-jump-target)
        factories (map (fn [binding target]
                         [(:name binding) (jump-factory case-block target)])
                       bindings
                       targets)
        clauses (map (fn [binding target]
                       [target `(fn ~(:arglist binding) ~@(:body binding))])
                     bindings
                     targets)
        e (gensym)]

    `(block return-block#
       (let [[case-clause# & args#]
             (block ~case-block
               (handler-bind [~@(mapcat identity factories)]
                 (return-from return-block# ~expr)))]
         (apply
          (case case-clause#
            ~@(mapcat identity clauses)
            (error ::control-error (str "Attempted to jump to invalid clause " case-clause#)))
          args#)))))
(s/fdef handler-case
  :args (s/cat :expr any?
               :bindings (s/* (s/spec ::handler-clause))))

(def ^:dynamic *restarts*
  "Dynamically-bound list of restarts."
  (list {::restart-name ::throw
         ::restart-reporter "Throw the condition as an exception"
         ::restart-interactive (constantly nil)
         ::restart-fn (fn [& args]
                        (throw (ex-info "Condition was thrown"
                                        (cond-> {}
                                          (first args) (assoc :condition (first args))
                                          (rest args) (assoc :arguments (rest args))))))}))

(s/def ::restart-name keyword?)
(s/def ::restart-fn ifn?)
(s/def ::restart-test ifn?)
(s/def ::restart-interactive ifn?)
(s/def ::restart-reporter ifn?)
(s/def ::restart-thread #?(:clj (partial instance? Thread)
                           :cljs keyword?))
(s/def ::restart (s/keys :req [::restart-name ::restart-fn]
                         :opt [::restart-test ::restart-interactive
                               ::restart-reporter ::restart-thread]))

(defmacro restart-bind
  "Runs the `body` with bound restarts.
  Within the dynamic scope of the `body`, [[invoke-restart]] may be called with
  any of the bound restart names. This includes inside handlers bound further up
  the stack.

  Each binding clause is one of the following forms:
  restart-name restart-fn
  restart-name [restart-fn & {:keys [test-function interactive-function]}]

  The restart-name can be any key for a map, but it is recommended to use a
  namespaced keyword.

  The restart-fn is a function of zero or more arguments, provided by rest
  arguments on the call to [[invoke-restart]]. The function returns normally.

  The test-function is a function of one optional argument, a condition. If it
  returns a truthy value, the restart is available, otherwise it cannot be
  invoked from its context. If not provided, the restart is assumed to be
  available.

  The interactive-function is a function of no arguments that is called to get
  input from the user interactively. It returns a list, used as the argument
  list to restart-fn."
  {:arglists '([[bindings*] exprs*])
   :style/indent [:defn]}
  [bindings & body]
  (let [bindings (map (fn [[k f]]
                        (if-not (vector? f)
                          {::restart-fn f
                           ::restart-name k}
                          (set/rename-keys
                           (assoc (apply hash-map (rest f))
                                  ::restart-fn (first f)
                                  ::restart-name k)
                           {:test-function ::restart-test
                            :interactive-function ::restart-interactive
                            :report-function ::restart-reporter})))
                      (reverse (partition 2 bindings)))]
    `(binding [*in-restartable-context* true
               *restarts* (into *restarts* (map #(assoc %
                                                        ::restart-thread
                                                         (macros/case
                                                             :clj (Thread/currentThread)
                                                             :cljs :unsupported))
                                                ~(cons 'list bindings)))]
       ~@body)))
(s/fdef restart-bind
  :args (s/cat :bindings
               (s/and (s/* (s/cat
                            :key keyword?
                            :restart (s/or :fn-with-opts vector?
                                           :bare-fn any?)))
                          vector?)
               :body (s/* any?)))

(s/def ::restart-clause (s/cat :name keyword?
                               :arglist vector?
                               :restart-fns (s/* (s/cat :keyword keyword?
                                                        :function any?))
                               :body (s/* any?)))

(defmacro restart-case
  "Runs the `expr` with bound restarts, returning a value from the restart on invoke.
  Bindings match [[restart-bind]].

  If one of the restarts bound in this case is invoked then the stack is
  immediately unwound to outside of `expr`, and then the restart is run, with
  its return value used as a replacement for its return value."
  {:arglists '([expr bindings*])
   :style/indent [:defn [:defn]]}
  [expr & bindings]
  (let [bindings (map (partial s/conform ::restart-clause) bindings)
        case-block (gensym)
        targets (repeatedly (count bindings) make-jump-target)
        factories (map (fn [binding target]
                         [(:name binding)
                          (apply vector (jump-factory case-block target)
                                 (mapcat identity
                                         (set/rename-keys (into {}
                                                                (map (juxt :keyword :function)
                                                                     (:restart-fns binding)))
                                                          {:test :test-function
                                                           :report :report-function
                                                           :interactive :interactive-function})))])
                       bindings
                       targets)
        clauses (map (fn [binding target]
                       [target `(fn ~(:arglist binding) ~@(:body binding))])
                     bindings
                     targets)
        e (gensym)]

    `(block return-block#
       (let [[case-clause# & args#]
             (block ~case-block
               (restart-bind [~@(mapcat identity factories)]
                 (return-from return-block# ~expr)))]
         (apply
          (case case-clause#
            ~@(mapcat identity clauses)
            (error ::control-error (str "Attempted to jump to invalid clause " case-clause#)))
          args#)))))
(s/fdef restart-case
  :args (s/cat :expr any?
               :bindings (s/* (s/spec ::restart-clause))))

(defmacro with-simple-restart
  "Constructs a restart with the given name which unwinds and returns nil.
  Returns true as a second value with [[values]] when the restart was
  triggered.

  The `format-str` and `args` are used when reporting the restart."
  [[restart-name format-str & args] & body]
  `(restart-case (values (do ~@body) nil)
     (~restart-name [] :report (fn [~'_] (format ~format-str ~@args)) :interactive (constantly nil)
      (values nil true))))

(defn handles-condition?
  "Returns true if the given `handler` can handle the `condition`."
  [condition handler]
  (boolean
   (or (isa? condition handler)
       #?(:clj (isa? (type condition) handler)))))
(s/fdef handles-condition?
  :args (s/cat :condition any?
               :handler (s/or :keyword keyword?
                              :class #?(:clj class?
                                        :cljs any?)))
  :ret boolean?)

(def ^:dynamic *debugger-hook*
  "Dynamically-bound hook called after signalling an [[error]] without a handler."
  nil)

(defn throwing-debugger
  "A \"debugger\" that wraps conditions with [[ex-info]] and throws them."
  [[condition & args] _]
  (throw (ex-info "Unhandled condition" {:condition condition
                                         :handlers (keys *handlers*)
                                         :args args}
                  (when (instance? #?(:clj Throwable
                                      :cljs js/Error)
                                   condition)
                    condition))))

(macros/case :clj
  (declare system-debugger))
(defn invoke-debugger
  "Calls the [[*debugger-hook*]], or a system debugger if not bound, with the `condition`.
  In Clojure the system debugger is [[system-debugger]]. In ClojureScript it
  is [[throwing-debugger]]."
  [condition & args]
  (if *debugger-hook*
    (let [hook *debugger-hook*]
      (binding [*debugger-hook* nil]
        (hook (cons condition args) hook)))
    (macros/case
        :clj (apply system-debugger condition args)
        :cljs (throwing-debugger (cons condition args) throwing-debugger))))

(defn break
  "Binds the system debugger and invokes it on the given condition."
  [condition & args]
  (binding [*debugger-hook* nil]
    (let [[condition & args] (if (string? condition)
                               (concat (list ::simple-condition condition) args)
                               (cons condition args))]
      (restart-case (apply invoke-debugger condition args)
        (::continue [] :report "Continue out of the debugger." :interactive (constantly nil))))))

(def ^:dynamic *break-on-signals*
  "Dynamically-bound type of signal to [[break]] on."
  nil)

(derive ::simple-condition ::condition)

(defn signal
  "Signals a condition, triggering handlers bound for the condition type.
  Looks for handlers which apply to the given `condition` using
  [[find-handlers]] and then applies them in sequence until they all complete or
  one calls [[invoke-restart]]. If this function returns normally, it will
  return nil.

  When [[*break-on-signals*]] is true, or `condition` matches it with [[isa?]],
  calls [[break]] before executing the signal."
  [condition & args]
  (let [[condition & args] (if (string? condition)
                             (concat (list ::simple-condition condition) args)
                             (cons condition args))
        condition-type (if (keyword? condition)
                         condition
                         (type condition))]
    (when-not (contains? (ancestors condition-type) ::condition)
      (derive condition-type ::condition))
    (when (or (true? *break-on-signals*)
              (isa? condition *break-on-signals*))
      (break "Breaking on signal %s, called with arguments %s" (pr-str condition-type) (pr-str args)))
    (loop [remaining-clusters *handlers*]
      (when (seq remaining-clusters)
        (binding [*handlers* (rest remaining-clusters)]
          (let [[thread cluster] (first remaining-clusters)]
            (when (or (= thread #?(:clj (Thread/currentThread)
                                   :cljs :unsupported))
                      (not thread))
              (doseq [[handler handler-fn] cluster
                      :when (handles-condition? condition handler)]
                (apply handler-fn condition args)))))
        (recur (rest remaining-clusters)))))
  nil)
(s/fdef signal
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(derive ::warning ::condition)
(derive ::simple-warning ::warning)

(defn warn
  "Signals a condition, printing a warning to [[*err*]] if not handled.
  Binds a restart called `:farolero.core/muffle-warning`, which can be invoked
  from any handlers to prevent the warning without any additional side effects.
  This restart may be invoked directly by calling [[muffle-warning]].

  The `condition` will be modified to derive from `:farolero.core/warning`. If
  it is a keyword, it will derive directly, otherwise it will derive the type.
  This allows general handlers of `:farolero.core/warning` to handle this
  condition.

  See [[signal]]."
  [condition & args]
  (let [[condition & args] (if (string? condition)
                             (concat (list ::simple-warning condition) args)
                             (cons condition args))
        condition-type (if (keyword? condition)
                         condition
                         (type condition))]
    (when-not (contains? (ancestors condition-type) ::warning)
      (derive condition-type ::warning))
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
      (::muffle-warning [] :report "Ignore the warning and continue" :interactive (constantly nil))))
  nil)
(s/fdef warn
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(derive ::error ::condition)
(derive ::simple-error ::error)
(macros/case
    :clj (derive Exception ::error)
    :cljs (derive js/Error ::error))

(defn error
  "Signals a condition, invoking [[*debugger-hook*]] if no handler is found.
  If [[*debugger-hook*]] is not bound to a function, this throws an [[ex-info]]
  with the message \"Unhandled condition\", and a map of the condition, bound
  handlers at the time of the error, and the arguments passed to the handler in
  the [[ex-data]].

  See [[signal]]."
  [condition & args]
  (let [[condition & args] (if (string? condition)
                             (concat (list ::simple-error condition) args)
                             (cons condition args))
        condition-type (if (keyword? condition)
                         condition
                         (type condition))]
    (when-not (contains? (ancestors condition-type) ::error)
      (derive condition-type ::error))
    (apply signal condition args)
    (apply invoke-debugger condition args)))
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
    (::continue [] :report "Ignore the error and continue" :interactive (constantly nil))))
(s/fdef cerror
  :args (s/cat :condition any?
               :args (s/* any?))
  :ret nil?)

(defn compute-restarts
  "Returns a sequence of all usable restarts.
  Any restart with a `:farolero.core/restart-test` function will be filtered
  based on if it returns a truthy value when called with `condition` and
  `args`."
  ([] (compute-restarts nil))
  ([condition & args]
   (filter #(and (or (= (::restart-thread %) #?(:clj (Thread/currentThread)
                                                :cljs :unsupported))
                     (not (::restart-thread %)))
                 (apply (::restart-test % (constantly true)) condition args))
           *restarts*)))
(s/fdef compute-restarts
  :args (s/cat :condition (s/? any?)
               :args (s/* any?))
  :ret (s/coll-of ::restart))

(defn find-restart
  "Returns the first restart bound named by `restart-name`."
  ([restart-name] (find-restart restart-name nil))
  ([restart-name condition & args]
   (first (filter (comp #{restart-name} ::restart-name) (apply compute-restarts condition args)))))
(s/fdef find-restart
  :args (s/cat :restart-name ::restart-name
               :condition (s/? any?)
               :args (s/* any?))
  :ret ::restart)

(defn invoke-restart
  "Calls a restart by the given name with `args`.
  If the restart isn't found, signals a `:farolero.core/control-error`.

  Throws an assertion exception if called outside a restart context.

  See [[restart-bind]], [[restart-case]]."
  [restart-name & args]
  (when-not *in-restartable-context*
    (error ::control-error "you must be inside a restartable context to invoke a restart"))
  (if-let [restart (if (keyword? restart-name)
                     (find-restart restart-name)
                     restart-name)]
    (apply (::restart-fn restart) args)
    (error ::control-error
           :type :missing-restart
           :restart-name restart-name
           :available-restarts (compute-restarts))))
(s/fdef invoke-restart
  :args (s/cat :restart-name (s/or :name keyword?
                                   :restart ::restart)
               :args (s/* any?)))

(defmacro wrap-exceptions
  "Catching all exceptions from evaluating `body` and signals them as [[error]]s.
  This only catches exceptions, meaning [[block]], [[tagbody]], conditions, and
  restarts can all be handled through the dynamic scope of `body` without
  issue."
  {:style/indent [:defn]}
  [& body]
  `(try ~@body
        (catch ~(macros/case :clj 'java.lang.Exception :cljs 'js/Error) e#
          (cerror e#))))
(s/fdef wrap-exceptions
  :args (s/cat :body (s/* any?)))

(defn invoke-restart-interactively
  "Calls a restart by the given name interactively.
  If the restart was created with an `:interactive-function`, then it is called
  to produce the argument list for the restart. Otherwise, a default is used. In
  Clojure, the default is to read and evaluate from [[*in*]]. In ClojureScript,
  the default is to produce nil as the arguments.

  See [[invoke-restart]]"
  [restart-name]
  (when-not *in-restartable-context*
    (error ::control-error "you must be inside a restartable context to invoke a restart"))
  (if-let [restart (if (keyword? restart-name)
                     (find-restart restart-name)
                     restart-name)]
    (apply invoke-restart restart-name
           ((or (::restart-interactive restart)
                #?(:clj #(restart-case (wrap-exceptions
                                         (eval (read)))
                           (::abort [] :report "Abort making the argument list and use nil")
                           (::use-value [v] :report "Uses the passed value for the argument list"
                             v))
                   :cljs (constantly nil)))))
    (error ::control-error
           :type :missing-restart
           :restart-name restart-name
           :available-restarts (compute-restarts))))
(s/fdef invoke-restart-interactively
  :args (s/cat :restart-name (s/or :name keyword?
                                   :restart ::restart)))

(defn muffle-warning
  "Ignores the warning and continues.
  Invokes the `:farolero.core/muffle-warning` restart."
  []
  (invoke-restart ::muffle-warning))
(s/fdef muffle-warning
  :ret nil?)

(defn continue
  "Ignores the signal and continues.
  Invokes the `:farolero.core/continue` restart.
  If the restart isn't present, returns nil."
  []
  (when-let [restart (find-restart ::continue)]
    (invoke-restart restart)))
(s/fdef continue
  :ret nil?)

(defn use-value
  "Uses `val` instead of the value which caused the error.
  Invokes the `:farolero.core/use-value` restart.
  If the restart isn't present, returns nil."
  [val]
  (when-let [restart (find-restart ::use-value)]
    (invoke-restart restart val)))
(s/fdef use-value
  :args (s/cat :val any?)
  :ret nil?)

(defn store-value
  "Stores the `val` in a way determined by the restart.
  Invokes the `:farolero.core/store-value` restart.
  If the restart isn't present, returns nil."
  [val]
  (when-let [restart (find-restart ::store-value)]
    (invoke-restart restart val)))
(s/fdef store-value
  :args (s/cat :val any?)
  :ret nil?)

(defn abort
  "Aborts the current computation.
  Invokes the `:farolero.core/abort` restart."
  []
  (invoke-restart ::abort))
(s/fdef abort
  :ret nil?)

(defmacro ignore-errors
  "Evaluates the `body`, returning nil if any errors are signalled."
  {:style/indent [:defn]}
  [& body]
  `(handler-case (do ~@body)
     (::error [& args#])))

(defmulti report-condition
  "Multimethod for creating a human-readable explanation of a condition."
  (fn [condition & args]
    (if (keyword? condition)
      condition
      (type condition))))
(s/fdef report-condition
  :args (s/cat :condition any?
               :args (s/* any?)))

(defmethod report-condition :default
  [condition & args]
  (str (pr-str (if (keyword? condition)
                 condition
                 (type condition)))
       " was signalled with arguments "
       (pr-str args)))

(macros/case
    :clj (defmethod report-condition Exception
           [condition & args]
           (ex-message condition)))

(defmethod report-condition ::simple-condition
  [_ format-str & args]
  #?(:clj (apply format format-str args)
     :cljs format-str))

(defmethod report-condition ::simple-warning
  [_ format-str & args]
  #?(:clj (apply format format-str args)
     :cljs format-str))

(defmethod report-condition ::simple-error
  [_ format-str & args]
  #?(:clj (apply format format-str args)
     :cljs format-str))

(macros/case :clj
  (do
    (defmacro ^:private with-abort-restart
      "Evaluates the `body` with an `:farolero.core/abort` restart bound."
      {:style/indent [:defn]}
      [& body]
      `(let [level# *debugger-level*]
         (with-simple-restart (::abort "Return to level %d of the debugger" level#)
           ~@body)))
    (s/fdef with-abort-restart
      :args (s/cat :body (s/* any?)))

    (def ^:dynamic ^:private *debugger-level*
      "Dynamic variable containing the current level of the system debugger."
      0)

    (def ^:dynamic *debugger-condition*
      "Dynamic variable with the condition currently signalled in the debugger."
      nil)

    (def ^:dynamic *debugger-arguments*
      "Dynamic variable with the args from the condition currently signalled in the debugger."
      nil)

    (defn- report-restart
      "Reports the restart using the the restart's reporter."
      [{:as restart ::keys [restart-name restart-reporter]}]
      (if restart-reporter
        (cond
          (string? restart-reporter) restart-reporter
          (ifn? restart-reporter) (restart-reporter restart))
        restart-name))
    (s/fdef report-restart
      :args (s/cat :restart ::restart))

    (defn system-debugger
      "Recursive debugger used as the default.
  Binds [[*debugger-level*]], [[*debugger-condition*]], and
  [[*debugger-arguments*]] inside the debugger.

  Reports the restarts available for the given condition, and provides a repl to
  evaluate arbitrary values to prepare for invoking them. Entering a number at
  the repl will invoke the corresponding restart interactively.

  If another error is signalled without being handled, an additional layer of
  the debugger is invoked."
      [condition & args]
      (binding [*debugger-hook* nil
                *debugger-level* (inc *debugger-level*)
                *debugger-condition* condition
                *debugger-arguments* args]
        (tagbody
         print-banner
         (println (str "Debugger level " *debugger-level* " entered on "
                       (if (keyword? condition)
                         condition
                         (type condition))
                       "\n"
                       (apply report-condition condition args)))
         (let [restarts (apply compute-restarts condition args)]
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
                   (if (and (number? form)
                            (< form (count restarts)))
                     form
                     (do (multiple-value-bind [[_ restarted?] (with-abort-restart
                                                                (wrap-exceptions
                                                                  (prn (eval form))))]
                           (when restarted?
                             (go print-banner)))
                         (prompt)
                         (recur (read)))))]
             (with-abort-restart
               (invoke-restart-interactively (nth restarts restart))
               (go print-banner))
             (go print-banner))))))))
