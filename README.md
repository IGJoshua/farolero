# farolero
![farolero](img/farolero.jpg)
> **farolero** masc. n.
>
> Historical Spanish, meaning "lamplighter", e.g. "A lamplighter claimed to have
> seen Jack the Ripper on this street last night."

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
{org.suskalo/farolero {:mvn/version "1.0.0-RC1"}}
```

Because this library relies on `gen-class`, it requires that you run the
`:build` alias if you wish to use it locally. Unfortunately this means you can't
use it as a git dependency, although you can download it locally, run the alias,
and use a local dependency.

## Usage
In this library there are three major components: conditions, handlers, and
restarts. Each one represents one of the three parts error handling is split
into when using this library. In places where an error might arise, you bind
restarts, named sections of code which provide ways to recover from an error.
When an error situation occurs, you `signal` a condition to notify the program
that an error has occurred. When a condition is `signal`ed, it will look up the
stack to find a place where a handler is bound and call it to decide what action
to take, whether to continue normally or to invoke one of the restarts bound for
that section of code. For example:

```clojure
(handler-bind [::some-condition (fn [condition]
                                  (invoke-restart ::some-restart))]
  (restart-bind [::some-restart (fn [] ::result)]
    (signal ::some-condition)))
```

The flow control of this acts as the following:
- call `signal`
- look up the stack to find a handler for `::some-condition`
- invoke that handler from inside `signal`
- call `invoke-restart`
- look up the stack to find a restart called `::some-restart`
- invoke that restart from inside the handler
- complete the restart and return

Conditions are either keywords or instances of some Java class. When a handler
is searched for that applies to the given condition, it will permit Clojure's
hierarchies to be used with keywords, and will pay attention to Java inheritance
for classes. The most specific handler will be run.

In order to signal a condition of some kind, four functions are provided:
`signal`, `warn`, `error`, and `cerror`. `signal` is the basic way to signal a
condition, and it implies that some situation that something higher on the stack
might want to influence the handling of has occurred, but if it is not handled
then `signal` will return nil. `warn` will signal a condition in the same
manner, but if it is not handled a warning is printed to `*err*` before it
returns nil. In addition it binds the restart `:farolero.core/muffle-warning` to
be used in a handler if the warning shouldn't be printed. `error` will signal a
condition, but will throw an exception if it is not handled. If the
`*error-hook*` dynamic variable is bound, then it will be called instead of
throwing an exception. `cerror` is like `error`, except that it binds the
restart `:farolero.core/continue` to be used in handlers, simply returns nil if
invoked.

Four macros act as the primary "entrypoints" to handling conditions and
restarts: `handler-bind`, `handler-case`, `restart-bind`, and `restart-case`.

Both of the `bind` macros simply bind the given handlers directly in their
dynamic scope for the body. If a condition is signaled in a `handler-bind` and
the handler for it returns normally, then the next handler for the condition is
invoked. This repeats until there are no more handlers, or one of them exits
without returning.

```clojure
(derive ::some-condition ::parent-condition)

(handler-bind [::some-condition (fn [condition] nil)
               ::parent-condition (fn [condition] (invoke-restart ::some-restart))]
  (restart-bind [::some-restart (fn [] ::hello)]
    (signal ::some-condition)))
```

If a restart is invoked inside `restart-bind`, then the restart function is
called with the rest arguments to the restart. The `invoke-restart` function
will return the result from the function normally. When combined with
`handler-bind` this produces little useful behavior since the handler which
invoked the restart will continue to execute normally, likely causing further
handlers to be called and the signal to return nil.

Generally, `restart-case` is used rather than `restart-bind`, whose primary
purpose is to aid in implementing macros which rely on restarts to function,
such as `restart-case`. If a restart is invoked inside `restart-case`, the stack
is immediately unwound outside of the expression before the restart code is run,
with its return value being used for the entire `restart-case` expression.

```clojure
(restart-case (do (invoke-restart ::some-restart)
                  (println "never reached"))
  (::some-restart []
    (println "reached!")
    ::return-result))
;; => ::return-result
```

`handler-case` mirrors `restart-case`, in that it immediately unwinds the stack
when a condition is signaled to outside of the expression, and then the handler
is run, with its return value used for the whole expression. This means that
there is no way to defer the handling of the error to a later handler by
returning normally.

```clojure
(handler-case (do (signal ::some-condition)
                  (println "never reached"))
  (::some-condition [condition]
    (println "reached!")
    ::return-result))
;; => ::return-result
```

## Laziness and Dynamic Scope
Condition handlers and restarts are bound only inside a particular dynamic
scope. Clojure provides facilities for deferring calculations to a later time
with things like `delay` and laziness. In order for a function which produces a
lazy sequence or other deferred calculation which relies on conditions to work
properly, you must ensure that any part of the calculation which is realized
must do so with handlers bound for the conditions it might signal, and restarts
bound for what it may invoke. The easiest way to ensure this is to fully realize
any data returned from functions which use conditions. A quick and dirty way to
do this which should work on any immutable Clojure data is to call `pr-str` on
the data, discarding the resulting string. This is the method used by
[special](https://github.com/clojureman/special), but it may fail when using
Java types or types which do not fully realize their values when printed. This
library does not attempt to force all of your functions to return fully-realized
data structures, but instead gives you the flexibility to realize things as you
like. Just be aware that if you are consistently receiving errors about
unhandled conditions when working from the repl, you may be having problems with
laziness.

## Multithreading
Both handlers and restarts are bound thread-locally, and do not carry over into
`future`s or `core.async/go` blocks, even with dynamic variable conveyance. This
is intentional. The semantics of a restart moving across thread boundaries is
difficult to determine in any case where a non-local return might occur, and any
handlers bound in a given dynamic context may attempt to invoke restarts without
awareness of which thread is calling them, and as a result, farolero simply
disallows handlers and restarts crossing thread boundaries.

When using libraries which add forms of concurrency besides simple threads
(core.async, promesa, manifold, etc.), care must be taken to ensure that code
run in the context of handlers and restarts is run on the same thread that bound
them. This means that, for example, in a core.async `go` block, you must not
park inside the dynamic scope of restarts or handlers if they are to be used.

In a case where you attempt to access a restart or handler which is not bound in
the current thread, a `:farolero.core/control-error` will be signaled.

## Known Issues
You may run into one of the issues below. I am aware of them and have plans to
fix them. If you know how to fix them or have the time, pull requests are always
welcome!

- In ClojureScript, warn and cerror do not correctly establish restarts (help is wanted on this one).

## License

Copyright © 2021 Joshua Suskalo

Distributed under the Eclipse Public License version 1.0.
