# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

New features:

Bugfixes:

Other improvements:

## [v8.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v8.0.0) - 2019-08-10

**Breaking changes**

 - #172 add `createRef` support @elliotdavies

## [v7.0.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v7.0.1) - 2019-05-27

- Relax upper bound on `purescript-typelevel-prelude` (@hdgarrood)

## [v7.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v7.0.0) - 2019-05-17

**Breaking Changes**

- #169 Bump dependency (@bbarker)

**Documentation**

- #165 Update documentation (@athanclark)

## [v6.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v6.1.0) - 2018-08-24

**Features**
 - #155 export react types (@tellnobody1)

## [v6.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v6.0.0) - 2018-06-09

**Breaking Changes**
- Alternative class construction #129 (@natefaubion)
- Replace ReactRender with IsReactElement #137 (@natefaubion)
- Event refactoring #144 (@ethul)
- Remove children for void DOM elements #145, #146 (@ethul)
- Updates for PureScript 0.12, including the Context API, and unsafe createElement variants #149 (@natefaubion)

**Features**
- Add onError #133 (@safareli)

## [v5.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v5.1.0) - 2017-12-16

**Features**

 - #130 Adds value array for multiselect (@tellnobody1)

## [v5.0.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v5.0.1) - 2017-11-21

**Fixes**

 - #125 `writeRef` writes directly to `this`.

## [v5.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v5.0.0) - 2017-11-02

**Breaking**

- #109 React 16 (@coot)
- #121 Fix event type functions (@spicydonuts)

## [v4.4.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.4.0) - 2017-10-11

**Features**

 - #100 refs (@coot)

## [v4.3.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.3.0) - 2017-09-06

**Features**

 - #114 Add common SVG elements (@evenchange4)

**Documentation**

 - #115 Update maintainers (@paf31)

## [v4.2.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.2.0) - 2017-08-29

**Features**

 - #112 Add SVG element `foreignObject` (@paulyoung)
 - #113 Add SVG attributes `x` and `y` (@paulyoung)

**Fixes**

 - #110 Update badge version in README (@coot)
 - #111 Correct documentation link (@nwolverson)

## [v4.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.1.0) - 2017-08-10

**Features**

- #103 Force update (@coot)
- #107 Export `childrenToArray` (@coot)

## [v4.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.0.0) - 2017-06-27

Updates for React 15 (@coot)

## [v3.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v3.0.0) - 2017-03-29

Updates for 0.11.1 compiler release.

## [v1.3.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.3.0) - 2016-09-17

Add simpler props-free versions of SVG functions (@joshuahhh)

## [v1.2.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.2.0) - 2016-09-11

Add `preventDefault` and `stopPropagation` (@teh).

## [v1.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.1.0) - 2016-06-12

Add a variant of `writeState` with a callback (@pkamenarsky)

## [v1.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.0.0) - 2016-06-11

- Updates for 1.0 core libraries and 0.9.1 compiler. This library will not work with compiler versions < 0.9.1. (@spicydonuts)

## [v0.7.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.7.1) - 2016-03-14

**Bug Fixes**

#70 - Fixes `aria` and `data` props

## [v0.7.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.7.0) - 2016-02-29

**Features**

#62 - Fix transform state (@spencerjanssen)
#63 - Stateless components with children (@ethul)

## [v0.6.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.6.0) - 2016-01-25

**Features**

#54 - Dynamic children support
#56 - Bindings for React 0.14

**Breaking Changes**

The `react` library is now explicitly required in the FFI code. `purescript-react` no longer assumes that `React` is globally available. Also, with the support for 0.14 bindings of React, the DOM bindings have been split out into a separate repository [purescript-react-dom](https://github.com/purescript-contrib/purescript-react-dom).

## [v0.5.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.5.0) - 2015-11-19

- Simplify effect types for read/write access.

## [v0.4.3](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.3) - 2015-10-18

Add `GetInitialState` argument in `spec'` (@ethul)

## [v0.4.2](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.2) - 2015-10-01

Fix inline styling error (@robkuz)

## [v0.4.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.1) - 2015-09-24

Fix a bug in `getChildren` (@ethul)

## [v0.4.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.0) - 2015-09-04

Add state and props to `ReactThis` (@ethul)

## [v0.3.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.3.0) - 2015-09-01

Support React 0.13.\* (@ethul)

## [v0.2.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.2.0) - 2015-08-31

Add additional arguments to lifecycle methods (@ethul)

## [v0.1.2](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.1.2) - 2015-08-12

Support `displayName` (@ethul)

## [v0.1.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.1.1) - 2015-07-29

Fix `bower.json` file.

## [v0.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.1.0) - 2015-07-02

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.
- Break up `React.DOM` module
- Make `this` reference explicit
