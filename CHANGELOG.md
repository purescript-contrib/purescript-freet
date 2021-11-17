# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#34 by @thomashoneyman)

## [v6.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#29)
- Added `CofreeT`, the cofree monad transformer, in a new module (`Control.Monad.Cofree.Trans`) (#26)

New features:

Bugfixes:

Other improvements:
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#22, #27)

## [v5.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v5.0.0) - 2019-08-26

- Added instance of `MonadAff` to the library, incurring a new dependency on `purescript-aff` (@thomashoneyman)

## [v4.1.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v4.1.0) - 2019-08-21

- Added `substFreeT` analogue of [`substFree`](https://github.com/purescript/purescript-free/blob/f686f5fc07766f3ca9abc83b47b6ad3da326759a/src/Control/Monad/Free.purs#L164) from `purescript-free` (@safareli)

## [v4.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v4.0.0) - 2018-05-24

- Updated for PureScript 0.12.0

## [v3.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v3.0.0) - 2017-04-02

- Updated for PureScript 0.11.1

## [v2.1.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v2.1.0) - 2017-01-31

- Added `Semigroup` and `Monoid` instances (@athanclark)

## [v2.0.1](https://github.com/purescript-contrib/purescript-freet/releases/tag/v2.0.1) - 2016-11-14

- Fixed shadowed name warning

## [v2.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v2.0.0) - 2016-10-17

- Updated dependencies

## [v1.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v1.0.0) - 2016-06-03

- Updated for 1.0 core libraries.

## [v0.3.1](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.3.1) - 2015-11-20

- Removed unused imports (@garyb)

## [v0.3.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.3.0) - 2015-09-18

- Bumped `transformers` dependency

## [v0.2.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.2.0) - 2015-08-26

- Bumped `transformers` dependency. This release works with compiler versions >= 0.7.4.

## [v0.1.3](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.3) - 2015-08-07

- Added `freeT` function

## [v0.1.2](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.2) - 2015-08-07

- Exported `resume`

## [v0.1.1](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.1) - 2015-08-07

- Added repository info for Pursuit

## [v0.1.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.0) - 2015-08-07

- Initial versioned release
