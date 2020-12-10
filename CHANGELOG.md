# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

- added the `CofreeT`, the cofree monad transformer in a new module (`Control.Monad.Cofree.Trans`) ([#26](https://github.com/purescript-contrib/purescript-freet/pull/26))

New features:

Bugfixes:

Other improvements:

## [v5.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v5.0.0) - 2019-08-26

Adds instance of `MonadAff` to the library, incurring a new dependency on `purescript-aff` (@thomashoneyman)

## [v4.1.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v4.1.0) - 2019-08-21

Adds `substFreeT` analogue of [`substFree`](https://github.com/purescript/purescript-free/blob/f686f5fc07766f3ca9abc83b47b6ad3da326759a/src/Control/Monad/Free.purs#L164) from `purescript-free` (@safareli)

## [v4.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v4.0.0) - 2018-05-24

Updates for 0.12.0

## [v3.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v3.0.0) - 2017-04-02

Updates for 0.11.1 compiler

## [v2.1.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v2.1.0) - 2017-01-31

- Added `Semigroup` and `Monoid` instances (@athanclark)

## [v2.0.1](https://github.com/purescript-contrib/purescript-freet/releases/tag/v2.0.1) - 2016-11-14

- Fixed shadowed name warning

## [v2.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v2.0.0) - 2016-10-17

- Updated dependencies

## [v1.0.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v1.0.0) - 2016-06-03

Updates for 1.0 core libraries.

## [v0.3.1](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.3.1) - 2015-11-20

Remove unused imports (@garyb)

## [v0.3.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.3.0) - 2015-09-18

Bump `transformers`.

## [v0.2.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.2.0) - 2015-08-26

Bump `transformers` dependency. This release works with compiler versions >= 0.7.4.

## [v0.1.3](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.3) - 2015-08-07

`freeT` function.

## [v0.1.2](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.2) - 2015-08-07

Export `resume`.

## [v0.1.1](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.1) - 2015-08-07

Add repository info for Pursuit.

## [v0.1.0](https://github.com/purescript-contrib/purescript-freet/releases/tag/v0.1.0) - 2015-08-07

Initial versioned release
