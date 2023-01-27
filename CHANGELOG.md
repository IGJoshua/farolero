# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

This project does not follow semantic versioning. Instead its versions increment in the following way:
- If the release contains only fixes and/or internal changes, a hotfix version is released
- If the release contains new features in the public api or minor changes which will not affect most users, a minor version is released
- If the release contains a huge number of new features, removes old features, or substantially changes the existing semantics of the library for most users, a major version is released
Any time a segment of the version number increments, all following segments are reset to zero.

## [Unreleased]
### Fixed
- Bug where non-keyword condition types would not correctly be handled by `*break-on-signals*`
- Bug where `throwing-debugger` would not correctly report handler names from non-exception conditions
- Condition report strings for exceptions would be nil if no message was provided, now the exception type is reported.
  The stack trace continues to not be a part of the report string, stack traces can be retrieved in other ways.

## [1.4.4] - 2022-07-19
### Fixed
- Bug where the exceptions thrown by `throwing-debugger` that came from non-exception conditions would throw an exception on attempting to print their stack traces
- Reflection warnings introduced in 1.4.1 from the jump target implementation change

## [1.4.3] - 2022-02-17
### Fixed
- Inconsistencies between internal implementation and specs for those internals

## [1.4.2] - 2022-02-14
### Fixed
- Error in the function spec for `block*`

## [1.4.1] - 2022-01-31
### Fixed
- Internal jump targets would consume memory as they were constructed, causing a slow memory leak
- Reflection warnings in the default system debugger
- Reflection and performance warnings resulting from the use of `tagbody`, `handler-case`, and `restart-case`

### Changed
- The spec on `make-jump-target` has been removed. The return value should not be depended on by user code.

## [1.4.0] - 2021-11-02
### Added
- New functions `request-value` and `request-interaction` to enable customizable interactive restarts

### Fixed
- The `block` macro previously introduced a `recur` point, which could cause confusion
- Attempting to unwind to a `block` no longer on the stack with `bound-fn` would throw a Signal rather than raising a `:farolero.core/control-error`
- clj-kondo hooks did not identify the correct recur points

## [1.3.1] - 2021-10-22
### Added
- Extension for integration with `fmnoise/flow`'s protocol

### Fixed
- `recur` forms in the clauses of `*-case` macros acted unexpectedly by establishing recur points

## [1.3.0] - 2021-09-01
### Added
- Ability to configure the interactive functions for `assert`, `check-type`, and `wrap-exceptions`

### Changed
- Default interactive-function changed to return nil in Clojure to bring behavior in line with ClojureScript and the Common Lisp spec

### Documentation
- Sweeping changes to docstrings to improve accuracy

### Fixed
- Internal function `handles-condition?` was public
- Incorrect specs for handler and restart names not requiring qualified keywords
- `tagbody` was not reentrant

## [1.2.0] - 2021-06-20
### Added
- Macros for unbinding handlers and restarts, `without-handlers`, and `without-restarts`

### Changed
- Handlers and restarts are no longer always thread-local. Uses of the `*-bind` macros allow you to specify, while `*-case` macros are always thread-local

### Fixed
- Exceptions thrown from handlers were incorrectly translated to signals
- Signaling with `:farolero.core/condition`, warning, and error threw an exception
- Documentation of test functions in `restart-bind` said it only took the condition

## [1.1.1] - 2021-05-20
### Added
- Support for [clj-kondo](https://github.com/clj-kondo/clj-kondo) linter by adding lint hooks

### Fixed
- Returning early from a binding expression of `multiple-value-bind` could not include multiple values
- `multiple-value-bind` could not return multiple values

## [1.1.0] - 2021-05-18
### Added
- Dynamic variable `*warning-printer*` to allow custom code for displaying warnings
- Support for `goog.string/format` style format strings in simple conditions in CLJS

### Fixed
- Exception types in CLJS not being correctly detected in handlers
- CLJ-only interactive functions being included in CLJS for `assert` and `check-type`

## [1.0.4] - 2021-05-17
### Fixed
- Bug where macros would be used at the incorrect time in CLJS

## [1.0.3] - 2021-05-13
### Changed
- The project no longer relies on gen-class, instead opting to have a JVM-only dependency

### Fixed
- More than one `:no-error` clause in `handler-case` was permitted
- The `:no-error` clause in `handler-case` was called even on an error

## [1.0.2] - 2021-04-19
### Fixed
- Exceptions were caught and converted to conditions around restarts
- The `wrap-exceptions` `use-value` restart required a list

## [1.0.1] - 2021-04-19
### Fixed
- No debugger was bound by default when `throwing-debugger` should have been
- The throwing debugger failed to throw argument-less exceptions unwrapped

## [1.0.0] - 2021-04-18
### Added
- A recursive system debugger with built-in repl and multithreading support
- Implementations of additional CL control flow ops like `tagbody`
- Readme with basic rationale and instructions for use
- Detailed documentation to all public functions
- Specs to all public functions
- Basic implementation of conditions and restarts

[Unreleased]: https://github.com/IGJoshua/farolero/compare/v1.4.4...develop
[1.4.4]: https://github.com/IGJoshua/farolero/compare/v1.4.3...v1.4.4
[1.4.3]: https://github.com/IGJoshua/farolero/compare/v1.4.2...v1.4.3
[1.4.2]: https://github.com/IGJoshua/farolero/compare/v1.4.1...v1.4.2
[1.4.1]: https://github.com/IGJoshua/farolero/compare/v1.4.0...v1.4.1
[1.4.0]: https://github.com/IGJoshua/farolero/compare/v1.3.1...v1.4.0
[1.3.1]: https://github.com/IGJoshua/farolero/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/IGJoshua/farolero/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/IGJoshua/farolero/compare/v1.1.1...v1.2.0
[1.1.1]: https://github.com/IGJoshua/farolero/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/IGJoshua/farolero/compare/v1.0.4...v1.1.0
[1.0.4]: https://github.com/IGJoshua/farolero/compare/v1.0.3...v1.0.4
[1.0.3]: https://github.com/IGJoshua/farolero/compare/v1.0.2...v1.0.3
[1.0.2]: https://github.com/IGJoshua/farolero/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/IGJoshua/farolero/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/IGJoshua/farolero/compare/e2f23793cbf91f7c6dc35e61028bd99c4578bb4a...v1.0.0
