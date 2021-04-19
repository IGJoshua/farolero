# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

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

[Unreleased]: https://github.com/IGJoshua/farolero/compare/v1.0.2...HEAD
[1.0.2]: https://github.com/IGJoshua/farolero/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/IGJoshua/farolero/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/IGJoshua/farolero/compare/e2f23793cbf91f7c6dc35e61028bd99c4578bb4a...v1.0.0
