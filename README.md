# farolero
![farolero](img/farolero.jpg)
> **farolero** masc. n.
>
> Historical Spanish, meaning "lamplighter", e.g. "A lamplighter claimed to have
> seen Jack the Ripper on this street last night."

[![cljdoc badge](https://cljdoc.org/badge/org.suskalo/farolero)](https://cljdoc.org/d/org.suskalo/farolero/CURRENT)
[![Clojars Project](https://img.shields.io/clojars/v/org.suskalo/farolero.svg)](https://clojars.org/org.suskalo/farolero)

Error handling in Clojure is not yet a solved problem. Each method of handling
errors commonly used comes with downsides. Representing error states with nil is
convenient for code structure, but prevents detailed error information from
being conveyed to the program outside of logs. The either monad requires special
syntax to be convenient for use and offers no options for error recovery.
Exceptions are the default way to handle errors in the JVM, but Clojure has no
easy way to extend the exception mechanism with new types, limiting how much
control you have over which errors you handle without re-throwing. Condition
libraries like [special](https://github.com/clojureman/special) give the
programmer tools for reporting errors but limited options in recovery, or break
in multithreaded contexts.

This library implements an improved version of these conditions, very close to
the spec defined for Common Lisp's conditions and restarts. This method of
handling errors follows the Clojure philosophy of decomplection by separating
error handling into three parts: reporting, reconciliation, and recovery.

## Installation
The library is available on Clojars. Just add the following to your `deps.edn`
file in the `:deps` key.

```
{org.suskalo/farolero {:mvn/version "1.4.0"}}
```

If you use [clj-kondo](https://github.com/clj-kondo/clj-kondo) then you may also
want to import the configuration and hooks included with the library. This can
be done by running the following command, and then adding
`"org.suskalo/farolero"` to the `:config-paths` key in your kondo config, as the
command recommends.

```
$ clj-kondo --copy-configs --dependencies --lint "$(clojure -Spath)"
Imported config to .clj-kondo/org.suskalo/farolero. To activate, add "org.suskalo/farolero" to :config-paths in .clj-kondo/config.edn.
```

## ClojureScript News
The 1.1.0 release was just released, and it represents a major milestone for
farolero. It includes a full test suite for ClojureScript support, and brings
the CLJS version of farolero almost up to feature parity with the CLJ version.

As of right now, the only thing that ClojureScript lacks significant support for
is the interactive debugger, because ClojureScript environments vary so much
from project to project. That said, you can still write your own custom
debuggers, but `assert` and `check-type` won't fully integrate with them at the
moment.

## Usage
In this library there are three major components: conditions, handlers, and
restarts. Each one represents one of the three parts error handling is split
into when using this library. In places where an error might arise, you bind
restarts, named sections of code which provide ways to recover from an error.

If you're an experienced Common Lisper, then most of this should be review, but
you may wish to skim further ahead to the code examples to see the few places
where the syntax differs.

### Handlers
Handlers are functions that are run when an error is encountered to decide how
to recover from the situation.

```clojure
(handler-case (signal ::signal)
  (::signal [condition]
    (println condition)
    (println "Handled the signal!")
    :result))
;; :user/signal
;; Handled the signal!
;; => :result
```

The macro `handler-case` executes the expression it's passed in a context where
the handlers below are called when a condition is signaled. In general,
`handler-case` is used when you can replace the entire expression wholesale with
the result from the handler. When a condition with a handler is signaled,
control flow is immediately passed out of the expression and to the handler.

```clojure
(handler-case (do (signal ::signal)
                  (println "Never reached"))
  (::signal [condition]
    (println "Handled the signal!")
    :result))
;; Handled the signal!
;; => :result
```

This construct acts very similarly to Java's `throw` and `catch`. However,
additional arguments beyond the condition can be passed to the handler.

```clojure
(handler-case (signal ::signal "world" :other-argument)
  (::signal [condition s v]
    (println "Hello," s)
    (prn v)))
;; Hello, world
;; :other-argument
;; => nil
```

This works through the entire dynamic scope of the expression passed, so the
signal may be made arbitrarily deep in the stack.

```clojure
(defn f
  []
  (signal ::signal :result))

(defn g
  []
  (f))

(handler-case (g)
  (::signal [condition res]
    res))
;; => :result
```

If a condition is signaled and there's no handler bound, then `signal` will
return nil.

```clojure
(signal ::signal)
;; => nil
```

### Conditions
Conditions are the values that get signaled. Namespaced keywords are used for
the default signals, but they aren't the only values which can be used. Any
object except for an un-namespaced keyword may be used as a signal.

```clojure
(handler-case (signal (RuntimeException. "An exception"))
  (Exception [ex]
    (println (.getMessage ex))
    :result))
;; An exception
;; => :result
```

This example also shows that handlers are applied with regard for inheritance.
This inheritance is both through Java's inheritance hierarchy, and also by
Clojure's default hierarchy.

```clojure
(handler-case (signal :farolero.core/simple-condition)
  (:farolero.core/condition [condition]
    :result))
;; => :result
```

When you call `signal` with any value, farolero will ensure that it derives from
`:farolero.core/condition`, at least indirectly. If the value derives from
`:farolero.core/condition` indirectly, then nothing changes.

```clojure
(contains? (ancestors ::random-condition) :farolero.core/condition)
;;  => false
(handler-case (signal ::random-condition)
  (:farolero.core/condition [condition]
    :result))
;; => :result
(contains? (ancestors ::random-condition) :farolero.core/condition)
;; => true
```

There are multiple ways to signal conditions with farolero. The way to signal
conditions we've used so far is `signal`. In addition there are `warn`, `error`,
and `cerror` (we'll talk about `cerror` when we discuss restarts).

```clojure
(handler-case (error ::random-error)
  (:farolero.core/error [condition]
    :result))
```

Conditions used for `warn` are made to derive `:farolero.core/warning`, and for
`error` and `cerror` the conditions derive `:farolero.core/error`. All Java
classes that extend from Exception also derive `:farolero.core/error`, and the
same for js/Error in ClojureScript.

When you know the return value to be used as a replacement for the whole
expression, `handler-case` is the way to bind a handler. However, in some cases
you may not want to abort execution of the expression in order to handle the
condition. In these cases, `handler-bind` is more appropriate.

```clojure
(handler-bind [::signal (fn [condition]
                          (println "In the condition handler."))]
  (signal ::signal))
;; In the condition handler.
;; => nil
```

If a handler bound in this way returns normally (rather than via e.g. `throw`),
then `signal` (and the other condition signaling functions) will keep searching
for another handler which applies.

```clojure
(handler-bind [:farolero.core/condition (fn [condition]
                                          (println "In outer handler"))]
  (handler-bind [::signal (fn [condition]
                            (println "In inner handler"))]
    (signal ::signal)))
;; In inner handler
;; In outer handler
;; => nil
```

If calling `warn` and all the handlers return normally, or no handler is found,
then the condition is printed to `*err*`.

```clojure
(warn "something went weird")
;; WARNING: :farolero.core/simple-warning signaled with arguments "something went weird"
;; => nil
```

### Restarts
Handlers give you a method of reacting to conditions when they are signaled.
Restarts provide a method of resuming the computation based on what environment
it's executing in. The macro `restart-case` mirrors `handler-case`, but with
`invoke-restart` taking the place of `signal`.

```clojure
(restart-case (invoke-restart ::restart)
  (::restart []
    (println "Invoked the restart!")
    :result))
;; Invoked the restart!
;; => :result
```

Unlike handlers, there is no inheritance between different restarts. Jumping to
a particular restart must be done by exact name, and only keywords can be used
as restart names.

Just like `handler-case`, invoking a restart in `restart-case` immediately
unwinds to outside of the expression and invokes the restart.

```clojure
(restart-case (do (invoke-restart ::restart)
                  (println "Never reached"))
  (::restart []
    (println "Invoked the restart!")
    :result))
;; Invoked the restart!
;; => :result
```

The `warn` and `cerror` functions each bind a restart that can be used by
handlers for the condition which gets signaled. The `warn` function binds
`:farolero.core/muffle-warning` (which can be called by the `muffle-warning`
function) which prevents the warning from being printed and continues execution
of the program.

```clojure
(handler-bind [::warning (fn [condition]
                           (muffle-warning))]
  (warn ::warning))
;; => nil
```

The `cerror` function binds a `:farolero.core/continue` restart (which can be
called by the `continue` function) which continues as if the error never
happened. The first argument to `cerror` is text that describes what ignoring
the error will do, and is used for interactive debugging.

```clojure
(handler-bind [::error (fn [condition]
                         (continue))]
  (cerror "Ignore the error" ::error))
;; => nil
```

When binding restarts, a test function can be provided that will be called to
test if the restart should be visible at any given time. This function must take
optional rest arguments for a condition the restart is being searched for in the
context of and its arguments.

```clojure
(restart-case (find-restart ::some-restart)
  (::some-restart [] :test (constantly nil)
    (println "Impossible to reach")))
;; => nil
```

As demonstrated above, `find-restart` may be called to find the first applicable
restart with a given name. You can call `invoke-restart` directly with its
return value instead of with the restart name to prevent the need to look it up
again.

The function `compute-restarts` returns a list of visible restarts, each value
of which includes a `:farolero.core/restart-name` key containing the restart's
name.

One restart is always bound, named `:farolero.core/throw`. It immediately throws
the condition using `ex-info`.

A dual to `restart-case` and mirror to `handler-bind` is `restart-bind`. It has
the same syntax as handler-bind, and when a restart is invoked, it is invoked as
a normal function and does not unwind the stack. This is generally not
particularly useful as if non-local transfer of control does not occur in the
restart, it will return to the code calling it, likely meaning that further
handlers will be invoked. The primary use of this macro is in the implementation
of additional facilities built atop restarts, such as `restart-case`.

### The Debugger
When `error` or `cerror` is called and no handler is bound for the condition
being signaled, the debugger is invoked using the function `invoke-debugger`.

```clojure
(restart-case (error ::ayy)
  (::some-restart [])
  (::some-other-restart []))
;; => throws an ex-info "Unhandled condition"
```

By default, the debugger will just throw the condition (wrapping it if it's not
already an exception). This enables library developers to use conditions without
requiring their users to learn farolero. For code that wants to use an
interactive debugger however, the following line should be included.

```clojure
(alter-var-root #'farolero.core/*debugger-hook* (constantly nil))
```

This will deactivate the debugger that throws exceptions, and allow farolero to
use the "system debugger" that is built in. This can, for example, be done
either at the top level or at runtime for an application, or in a namespace
loaded only during development (like `user`) for a library.

```clojure
(restart-case (error ::ayy)
  (::some-restart [])
  (::some-other-restart []))
;; Debugger level 1 entered on :user/ayy
;; :user/ayy was signaled with arguments nil
;; 0 [:user/some-restart] :user/some-restart
;; 1 [:user/some-other-restart] :user/some-other-restart
;; 2 [:farolero.core/throw] Throw the condition as an exception
;; user> 0
;; => nil
```

When the system debugger is invoked, it reports the condition which triggered
it, and lists the restarts available in the current context. If you enter a
simple number that's an index of one of the available restarts, then that
restart will be invoked interactively, prompting the user for input. If the
restart has no special handling for being invoked interactively, as the restarts
above, a default interactive handler will be used.

Instead of using a number, arbitrary expressions may be evaluated at the
debugger before providing a restart to continue with. This may be used to get
the program into a state where the error may be continued from without issues.

If any more unhandled errors arise during the debugger's evaluation, then an
additional recursive layer of the debugger is invoked.

```clojure
(error ::ayy)
;; Debugger level 1 entered on :user/ayy
;; :user/ayy was signaled with arguments nil
;; 0 [:farolero.core/throw] Throw the condition as an exception
;; user> (error "oy")
;; Debugger level 2 entered on :farolero.core/simple-error
;; oy
;; 0 [:farolero.core/abort] Return to level 1 of the debugger
;; 1 [:farolero.core/throw] Throw the condition as an exception
;; user>
```

When inside recursive layers of the debugger, the `:farolero.core/abort` restart
is bound, allowing you to return to higher levels of the debugger and work from
there.

The debugger and interactive restarts use `*in*` and `*out*` for input and
output, but many interactive restarts also signal conditions to request the data
they need and allow it to be supplied by using a `:farolero.core/use-value`
restart.

In some contexts, it may be desirable to have alternative behavior when
conditions are raised without an applicable handler, rather than invoking the
default interactive debugger (e.g. writing a custom GUI debugger). The dynamic
variable `*debugger-hook*` can be bound to change the behavior of
`invoke-debugger`. The default value for the hook is `throwing-debugger`, which
is a function that will throw any conditions it is invoked with.

When making custom debuggers, the user binds a function to the hook. The bound
function must take two arguments, first a list of the condition and its
arguments, and the second is the currently bound debugger hook, which should be
used to invoke the debugger again rather than calling `invoke-debugger`
directly, or to bind `*debugger-hook*` again before calling other code, as
`invoke-debugger` unbinds the hook before calling it, so that if an error is
raised in it the system debugger will be invoked instead.

If the `*debugger-hook*` is bound to nil, it will invoke the system debugger,
which by default is the debugger described above. The `*system-debugger*`
dynamic variable contains the debugger to be called in this situation. This
variable should never be bound to nil.

The `break` function can be used to create breakpoints in your code. When
called, it binds `*debugger-hook*` to nil before calling `invoke-debugger`,
ensuring the system debugger is used. This allows the primary debugger to be one
which automatically handles errors, such as `throwing-debugger`, but when
`break` is called, the system debugger will be invoked, allowing the user to
interactively debug the application before resuming execution.

When binding restarts, additional information can be provided for use with the
debugger. A report function can be provided, as well as a function invoked to
interactively request any needed arguments to the restart function.

```clojure
(restart-case (error ::ayy)
  (::some-restart [x]
    :report (fn [restart] (str "Value for some restart"))
    :interactive (constantly (list 5))
    x))
;; Debugger level 1 entered on :user/ayy
;; :user/ayy was signaled with arguments nil
;; 0 [:user/some-restart] Value for some restart
;; 1 [:farolero.core/throw] Throw the condition as an exception
;; user> 0
;; => 5
```

### Applications
With an understanding of *what* conditions and restarts are, and *how* to use
them, there remains the question of *when* they should be applied.

The basic rule of thumb is any time there's more than one way to handle a
situation, you bind some restarts and signal a condition. For a more concrete
look at the kinds of situations this may occur in, and how this can improve your
code, take a look at the [example projects](./doc/examples.md).

### Library Developers
When writing libraries with farolero, it may be desirable to not require the
user to have experience with farolero, instead allowing them to use more
familiar methods of error handling.

In these cases, farolero's default debugger will aid the library developer. When
an error is signaled and not handled, the debugger is invoked. In general, the
user will be the one who decides which debugger will be used, but if they don't
use farolero directly, it will be left as the default, which will throw the
condition as an exception.

In order to aid in exception handling in your public api, errors should be
signaled as exceptions with no additional arguments.

```clojure
(error (RuntimeException. "an error"))
;; => throws a RuntimeException
```

If any additional arguments are signaled along with the condition, or if
something other than an exception is signaled, then the value will be wrapped in
an `ex-info`.

```clojure
(error (RuntimeException. "an error") ::some-arg)
;; => throws an ex-info with a RuntimeException cause
(error ::some-condition)
;; => throws an ex-info
```

The `ex-info` will have the keys `:condition`, containing the signaled value,
and `:args` containing a seq of the rest of the arguments.

If you desire to provide dynamic variables for handlers and restarts to provide
an interface similar to the library-less approach, it can be accomplished
relatively simply, while handing off handling of unwinding to farolero.

```clojure
;; A restart
(def ^:dynamic *use-value*)
(def ^:dynamic *some-handler* (constantly nil))

(defn some-func 
  []
  (restart-case
      (binding [*use-value* (fn [v] (use-value v))]
        (do-some-stuff)
        (signal ::some-condition))
    (:farolero.core/use-value [v]
      v)))

(defn library-entrypoint
  []
  (handler-bind [::some-condition *some-handler*]
    (some-func)))

;; in user code
(binding [*some-handler* (fn [condition] (*use-value* :blah))]
  (library-entrypoint))
;; => :blah
```

This requires placing a handler-bind around all of the entrypoints of the
library. If the user decides to use farolero directly instead of this approach,
then having the handlers be bound to a function that returns nil will cause
farolero to look further up the stack for a handler, meaning the user can bind
their own handlers if desired.

An additional thing that a library developer should consider when writing code
with farolero is that interactive functions, the functions used to get the
arguments for an interactive restart, should be configurable by the library user
so that they can provide a custom debugger that will be able to interact with
your restarts, but then have a default way of fetching user input as well. The
function `request-value` is provided to make this easy.

``` clojure
(restart-case (invoke-restart-interactively ::some-restart)
  (::some-restart [a]
    :interactive #(list (request-value ::interactive-some-restart))
    a))
```

This will first signal `::interactive-some-restart` to allow a handler to
provide a value with the `:farolero.core/use-value` restart, and then if they do
not, present a repl-like interface reading and writing with `*in*` and `*out*`.
This is the correct way to handle interactive functions to allow user
customizability, without requiring the library user to define something special
if they are willing to use the default experience.

The specific reason for this pattern, as opposed to the Common Lisp pattern of
using streams for debug io, is to prevent needlessly serializing and
deserializing data as it is sent up and down the stack.

`request-value` will ensure that the condition signaled also derives from
`:farolero.core/request-value`, allowing a handler to be bound to deal with
every instance of an interactive value request.

If some kind of interaction needs to be performed but no value returned, use
the function `request-interaction`.

```clojure
(restart-case (invoke-restart-interactively ::some-restart)
  (::some-restart []
    :interactive #(request-interaction ::interactive-some-restart)
    5))
```

This will ensure that `::interactive-some-restart` derives from
`:farolero.core/request-interaction`. The parent type for all interaction and
value requests is `:farolero.core/interaction`.

If you wish to provide a custom default handler instead of the included repl (as
e.g. `farolero.core/assert` does), then follow this pattern:

```clojure
;; At the top level somewhere
(derive ::interactive-some-restart :farolero.core/request-interaction)

;; In your error handling
(restart-case (invoke-restart-interactively ::some-restart)
  (::some-restart []
    :interactive
    (fn []
      (restart-case
          (do
            (signal ::interactive-some-restart)
            ;; do some things that are the default
            )
        (:farolero.core/continue [])))
    5))
```

If the handler requires a value, then use the `:farolero.core/use-value`
restart, and derive your condition from `:farolero.core/request-value`.

### Laziness and Dynamic Scope
Condition handlers and restarts are bound only inside a particular dynamic
scope. This can create some challenges with the facilities that Clojure provides
for deferring calculations, like `delay` and laziness.

```clojure
(handler-bind [:farolero.core/condition
               (fn [& args]
                 (apply prn args)
                 (continue))]
  (delay (cerror "hello")))
;; => #<Delay@28c6c817: :not-delivered>
@*1
;; => Unhandled condition

(handler-bind [:farolero.core/condition
               (fn [& args]
                 (apply prn args)
                 (continue))]
  (map cerror ["hello"]))
;; => Unhandled condition
```

These sorts of problems can be frustrating to deal with, and hard to find. The
reason for them comes from the way that Clojure evaluates this code. In the case
of `delay`, this is fairly clear what's happening. While the code is inside the
delay, it's only actually run when we dereference the returned value. This makes
it clear that the code is run outside of the dynamic extent of the
`handler-bind`.

The case with `map` is a little harder to see, especially for new users of
Clojure, and especially at the repl. What's happening is that `map` produces a
lazy sequence, which does not evaluate the function that it calls on the
sequence when you call `map`. Instead, the function passed to `map` is only
called when the lazy sequence is consumed. This is somewhat confused by the fact
that the repl will consume the sequence implicitly as it prints the value.

Because this printing happens after the expression has already returned, it
means that it's outside of the dynamic extent of the `handler-bind`.

All is not lost, however. We have multiple ways we can deal with this problem.
First off, in the case of `map`, we could simply fully realize the sequence when
we create it, by using `mapv` or `doall`, and in some situations using `pr-str`
and discarding the string will be helpful because it will perform the
realization deeply.

```clojure
(handler-bind [:farolero.core/condition
               (fn [& args]
                 (apply prn args)
                 (continue))]
  (doall (map cerror ["hello"])))
;; (:farolero.core/simple-error "An error has occurred")
;; => (nil)
```

This won't work if you need to keep the laziness of your sequence, due to side
effects or memory constraints, and it won't help in the case of `delay` either.
In those situations, you can use `bound-fn`.

```clojure
(handler-bind [:farolero.core/condition
               (fn [& args]
                 (apply prn args)
                 (continue))]
  (map (bound-fn [s] (cerror s)) ["hello"]))
;; (:farolero.core/simple-error "An error has occurred")
;; => (nil)
```

`bound-fn` will capture the dynamic context when it's evaluated, ensuring that
the body has the correct handlers and restarts bound when it's called. This
however has a limitation on certain handlers and restarts, as you can only
unwind to a point on the stack if that point is still on the stack.

```clojure
(let [f (block bad
          (bound-fn [] (return-from bad)))]
  (f))
;; => Signals a :farolero.core/control-error
```

This fails because by the time we call `f`, the block it attempts to return from
is not on the stack anymore. In these cases a `:farolero.core/control-error` is
signaled, invoking the debugger and giving you information about the failure.

### Multithreading
Handlers and restarts are bound thread-locally, but with dynamic variable
conveyance they may carry over to other threads in some contexts. To deal with
this, farolero allows the user to specify whether a particular handler or
restart is not thread-local when calling `handler-bind` or `restart-bind`.

```clojure
user=> (handler-bind [::foo (fn [c] (println c))]
         @(future (signal ::foo)))
;; :user/foo
;; => nil
user=> (handler-bind [::foo [(fn [c] (println c)) :thread-local true]]
         @(future (signal ::foo)))
;; => nil
user=> (restart-bind [::foo (fn [])]
         @(future (find-restart ::foo)))
;; => #:farolero.core{:restart-name ::foo}
user=> (restart-bind [::foo [(fn []) :thread-local true]]
         @(future (find-restart ::foo)))
;; => nil
```

If a handler or restart is labeled as thread-local, then it is simply not
visible to other threads, and they will continue to search further up the stack.

```clojure
user=> (handler-bind [::foo (fn [_] (println "outer"))]
         (handler-bind [::foo [(fn [_] (println "inner")) :thread-local true]]
           @(future (signal ::foo))))
;; outer
;; => nil
user=> (restart-bind [::foo (fn [] (println "outer"))]
         (restart-bind [::foo [(fn [] (println "inner")) :thread-local true]]
           @(future (invoke-restart ::foo))))
;; outer
;; => nil
```

In contrast to the `*-bind` macros, `handler-case` and `restart-case` always
bind thread-local handlers and restarts, because they always unwind the stack to
a particular point.

```clojure
user=> (handler-case (signal ::foo)
         (::foo [c]
           (println c)))
;; :user/foo
;; => nil
user=> (handler-case @(future (signal ::foo))
         (::foo [c]
           (println c)))
;; => nil
```

When using libraries which add forms of concurrency besides simple threads
(core.async, promesa, manifold, etc.), care must be taken to ensure that code
run in the context of thread-local handlers and restarts is run on the same
thread that bound them. This means that, for example, in a core.async `go`
block, you must not park inside the dynamic scope of thread-local restarts or
handlers if they are to be used.

In a case where you attempt to access a restart which is not bound in the
current thread, a `:farolero.core/control-error` will be signaled.

The system debugger included with farolero also supports multithreaded contexts.
If the debugger is invoked from a thread while it is already active, it will be
queued for later use. If the user wishes to switch which debugger is active
while debugging, they may enter `:switch-debugger` at the repl, followed by the
index of the debugger they wish to switch to. If something other than a number
is read, a control error is signaled with restarts bound to retry and to abort
and go back to the debugger you started from.

```clojure
user=> (error "Error from thread 1")
;; Debugger level 1 entered on :farolero.core/simple-error
;; Error from thread 1
;; 0 [:farolero.core/throw] Throw the condition as an exception
;; user> (future (error "Error from thread 2"))
;; #object[clojure.core$future_call$reify__8477 0x646c0a67 {:status :pending, :val nil}]
;; user> :switch-debugger
;; Debuggers from other threads
;; 0 [clojure-agent-send-off-pool-0] Error from thread 2
;; Debugger to activate: 0
;; Debugger level 1 entered on :farolero.core/simple-error
;; Error from thread 2
;; 0 [:farolero.core/throw] Throw the condition as an exception
;; user> 0
;; Debugger level 1 entered on :farolero.core/simple-error
;; Error from thread 1
;; 0 [:farolero.core/throw] Throw the condition as an exception
;; user> 0
;; Execution error (ExceptionInfo) at farolero.core/fn (core.cljc:315).
;; Condition was thrown
user=>
```

### Other Control Flow
In addition to the core functions and macros required to make conditions and
restarts, farolero provides a few more control flow operators inspired by the
Common Lisp spec.

The `block` macro (and its paired `block*` function) provides a way to perform
an early return from a named block.

```clojure
(block the-block
  (return-from the-block :hello)
  :goodbye)
;; => :hello
```

Passing no second argument to `return-from` results in the `block` returning nil.

```clojure
(block the-block
  (return-from the-block)
  :goodbye)
;; => nil
```

This return works anywhere within the dynamic scope of the block, not just
within its current stack frame.

```clojure
(defn some-func
  [f]
  (f :hello)
  :goodbye)

(block the-block
  (some-func #(return-from the-block %))
  :goodbye)
;; => :hello
```

If you use a keyword instead of a symbol, then `return-from` will unwind the
stack until the first `block` which uses the same keyword. This is equivalent to
Common Lisp's `throw` and `catch`.

```clojure
(defn throwing-func
  []
  (return-from :the-block :goodbye))

(block :the-block
  (block :the-block
    (throwing-func)) ;; => :goodbye
  :hello)
;; => :hello
```

The `block*` function calls a closure in the context of such a block with the
given keyword as the block name.

```clojure
(block* :the-block
  #(do (return-from :the-block :hello)
       :goodbye))
;; => :hello
```

If you want to uniquely specify a block name for use with `block*`, the
`make-jump-target` function is provided.

```clojure
(let [the-block (make-jump-target)]
  (block* the-block
    #(do (return-from the-block :hello)
         :goodbye)))
;; => :hello
```

Any extra arguments passed to `block*` are passed as arguments to the closure.

```clojure
(block* :the-block
  #(do (return-from :the-block %)
       :goodbye)
  :hello)
;; => :hello
```

If you attempt to `return-from` a block that isn't in the current thread's
dynamic scope, then a `:farolero.core/control-error` is signaled.

```clojure
(return-from :error nil)
;; => Entered the debugger on :farolero.core/control-error
```

An additional facility is `tagbody`, which binds labels for its dynamic scope
which can be jumped to with `go`. This is more or less an imperative `letfn`,
but can be used to implement more complex control flow than the other operators
in Clojure.

```clojure
(let [x (volatile! 0)]
  (tagbody
    (println "Entered tagbody!")
    loop
    (when (> @x 5)
      (go exit))
    (vswap! x inc)
    (go loop)
    exit
    (println "Exiting tagbody!"))
  @x)
;; => 6
```

The `tagbody` clause always returns nil.

Just like `block` and `return-from`, `go` may be used anywhere within the
dynamic scope of the `tagbody`.

```clojure
(defn call-if-greater
  [v f]
  (when (> v 5)
    (f)))

(let [x (volatile! 0)]
  (tagbody
    (println "Entered tagbody!")
    loop
    (call-if-greater @x #(go exit))
    (vswap! x inc)
    (go loop)
    exit
    (println "Exiting tagbody!"))
  @x)
;; => 6
```

This can be combined with `block` to add a return value.

```clojure
(let [x (volatile! 0)]
  (block the-block
    (tagbody
      (println "Entered tagbody!")
      loop
      (when (> @x 5)
        (go exit))
      (vswap! x inc)
      (go loop)
      exit
      (println "Exiting tagbody!")
      (return-from the-block @x))))
;; => 6
```

When using `restart-case`, `tagbody` can be used to provide a way to retry items from the restarts.

```clojure
(block exit
  (tagbody
    retry
    (return-from exit
      (restart-case (if (some-condition?)
                      (invoke-restart :farolero.core/continue)
                      :eventual-result)
        (:farolero.core/continue []
          (go retry))))))
```

The above code will either loop infinitely as `some-condition?` returns true
repeatedly, or it will eventually return `:eventual-result` if it ever returns
false.

### Implementation Caveat
Many different operators in farolero build upon the `block` macro and its
associated functions. The `block` macro is implemented in terms of the JVM's
exception mechanism, by throwing a value that extends `java.lang.Error`. This
value specifies a particular `block` that it unwinds to. The purpose of the
`java.lang.Error` class is to provide a way to throw a value that is explicitly
intended not to be caught.

Unfortunately you may sometimes see code that catches `java.lang.Throwable`. In
nearly all cases, this code doesn't need to and shouldn't catch this much, and
the primary reason to do it is to allow the code to catch both all
`java.lang.Exception`s, and `java.lang.AssertionError`.

What this means however is that in cases where code catches all `Throwable`s
farolero will be unable to unwind the stack past that boundary, and if the value
is logged, it may be confusing as farolero's `Signal` class does not include a
stack trace or error message.

The reality of the situation is that while farolero can do nothing about this
(except in cases where extension mechanisms are provided, as with
[flow](https://github.com/fmnoise/flow)), many pieces of code that catch
`Throwable` are frameworks of various sorts, and it's unlikely to desire
unwinding past them, so this rarely is an issue, but it is one that you should
keep in mind when using farolero.

### Extensions
Some other error handling libraries will try to interact with exceptions by
catching `Throwable`, which will interfere with the farolero unwind mechanism.
Thankfully, some of those libraries also provide extension mechanisms to specify
behavior for particular exceptions, which gives farolero a way to keep the
unwind mechanism functional. In cases like this, farolero adds an extension
namespace.

When working with JVM Clojure, this will operate transparently to the user, as
the libraries will be detected at runtime and extensions loaded. Unfortunately,
ClojureScript doesn't provide a mechanism for checking for dependencies at
runtime, and this means that you will have to require the extension namespace
yourself to activate the integration.

The namespace names for extensions are of the form
`farolero.extensions.lib-name`, like `farolero.extensions.flow` for integration
with [flow](https://github.com/fmnoise/flow).

The following libraries currently have extensions:

- [flow](https://github.com/fmnoise/flow)

## Known Issues
You may run into one of the issues below. I am aware of them and have plans to
fix them. If you know how to fix them or have the time, pull requests are always
welcome!

## License

Copyright © 2021 Joshua Suskalo

Distributed under the Eclipse Public License version 1.0.
