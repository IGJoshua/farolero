# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [1.1.0]
### Added
- Dynamic variable `*warning-printer*` to allow custom code for displaying warnings
- Support for `goog.string/format` style format strings in simple conditions in CLJS

### Fixed
- Exception types in CLJS not being correctly detected in handlers
- CLJ-only interactive functions being included in CLJS for `assert` and `check-type`

## [1.0.4]
### Fixed
- Bug where macros would be used at the incorrect time in CLJS

## [1.0.3]
### Changed
- The project no longer relies on gen-class, instead opting to have a JVM-only dependency.

### Fixed
- More than one `:no-error` clause in `handler-case` was permitted
- The `:no-error` clause in `handler-case` was called even on an error

## [1.0.2]
### Fixed
- Exceptions were caught and converted to conditions around restarts
- The `wrap-exceptions` `use-value` restart required a list

## [1.0.1]
### Fixed
- No debugger was bound by default when `throwing-debugger` should have been
- The throwing debugger failed to throw argument-less exceptions unwrapped

## [1.0.0]
### Added
- A recursive system debugger with built-in repl and multithreading support
- Implementations of additional CL control flow ops like `tagbody`
- Readme with basic rationale and instructions for use
- Detailed documentation to all public functions
- Specs to all public functions
- Basic implementation of conditions and restarts

[1.1.0]: https://github.com/IGJoshua/farolero/compare/v1.0.4...v1.1.0
[1.0.4]: https://github.com/IGJoshua/farolero/compare/v1.0.3...v1.0.4
[1.0.3]: https://github.com/IGJoshua/farolero/compare/v1.0.2...v1.0.3
[1.0.2]: https://github.com/IGJoshua/farolero/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/IGJoshua/farolero/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/IGJoshua/farolero/compare/e2f23793cbf91f7c6dc35e61028bd99c4578bb4a...v1.0.0
