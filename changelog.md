# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [TODO]
- Debian package.
- Replace .ss with .jpg instead of appending.
- Manual.
- OSX port.
- Windows port.
- Tests for good and bad inputs.
- Use stderr for errors.

## [Unreleased]

### Added
- Extract JPEG image out of FF15 Photo Snapshot files (.ss).
- Takes exactly one file as input.
- Extracts JPEG image out of it.
- Puts image under the name input-file-name.ss.jpg.
- Takes directory as input and processes .ss files inside.
- Returns 0 if all is well.
- Prints error on stdout and exits with 1 if something failed.

## [0.0.1] - 2018-MM-DD
### Added
- TODO make my first release

[Unreleased]: https://github.com/TODO
[0.0.1]: https://github.com/TODO

