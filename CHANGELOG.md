# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v10.0.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v10.0.1) - 2022-04-27

Other improvements:
- Minifier-friendly refereces to properties (#183 by @sd-yip)

## [v10.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v10.0.0) - 2022-04-27

Breaking changes:
- Migrate FFI to ES modules (#185 by @JordanMartinez)
- Replaced polymorphic proxies with monomorphic `Proxy` (#185 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#182 by @thomashoneyman)

## [v9.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v9.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#177)

New features:
- Added roles declarations to allow safe coercions (#175)

Bugfixes:

Other improvements:
- Changed default branch to `main` from `master`
- Updated related modules and installation in README (#174)
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#176)

## [v8.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v8.0.0) - 2019-08-10

Breaking changes:

- Added `createRef` support (#172 by @elliotdavies)

## [v7.0.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v7.0.1) - 2019-05-27

- Relaxed upper bound on `purescript-typelevel-prelude` (@hdgarrood)

## [v7.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v7.0.0) - 2019-05-17

Breaking changes:

- Bumped dependencies (#169 by @bbarker)

Documentation:

- Updated documentation (#165 by @athanclark)

## [v6.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v6.1.0) - 2018-08-24

New features:

- Exported React types (#155, @tellnobody1)

## [v6.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v6.0.0) - 2018-06-09

Breaking changes:

- Alternative class construction (#129 by @natefaubion)
- Replace ReactRender with IsReactElement (#137 by @natefaubion)
- Event refactoring (#144 by @ethul)
- Remove children for void DOM elements (#145 and #146 by @ethul)
- Updated for PureScript 0.12, including the Context API, and unsafe createElement variants (#149 by @natefaubion)

New features:

- Added `onError` (#133 by @safareli)

## [v5.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v5.1.0) - 2017-12-16

New features:

- Added value array for multiselect (#130 by @tellnobody1)

## [v5.0.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v5.0.1) - 2017-11-21

Bugfixes:

- `writeRef` writes directly to `this` (#125)

## [v5.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v5.0.0) - 2017-11-02

Breaking changes:

- React 16 (#109 by @coot)
- Fixed event type functions (#121 by @spicydonuts)

## [v4.4.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.4.0) - 2017-10-11

New features:

- Support refs(#100 by @coot)

## [v4.3.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.3.0) - 2017-09-06

New features:

- Add common SVG elements (#114 by @evenchange4)

Other improvements:

- Updated maintainers (#115 by @paf31)

## [v4.2.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.2.0) - 2017-08-29

New features:

- Added SVG element `foreignObject` (#112 by @paulyoung)
- Added SVG attributes `x` and `y` (#113 by @paulyoung)

Bugfixes:

- Update badge version in README (#110 by @coot)
- Correct documentation link (#111 by @nwolverson)

## [v4.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.1.0) - 2017-08-10

New features:

- Force update (#103 by @coot)
- Export `childrenToArray` (#107 by @coot)

## [v4.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v4.0.0) - 2017-06-27

- Updated for React 15 (@coot)

## [v3.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v3.0.0) - 2017-03-29

- Updated for PureScript 0.11.1.

## [v1.3.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.3.0) - 2016-09-17

- Added simpler props-free versions of SVG functions (@joshuahhh)

## [v1.2.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.2.0) - 2016-09-11

- Added `preventDefault` and `stopPropagation` (@teh).

## [v1.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.1.0) - 2016-06-12

- Added a variant of `writeState` with a callback (@pkamenarsky)

## [v1.0.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v1.0.0) - 2016-06-11

- Updates for 1.0 core libraries and 0.9.1 compiler. This library will not work with compiler versions < 0.9.1. (@spicydonuts)

## [v0.7.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.7.1) - 2016-03-14

Bugfixes

- Fixed `aria` and `data` props (#70)

## [v0.7.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.7.0) - 2016-02-29

New features:

- Fix transform state (#62 by @spencerjanssen)
- Stateless components with children (#63 by @ethul)

## [v0.6.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.6.0) - 2016-01-25

New features:

- Added support for dynamic children (#54)
- Updated bindings for React 0.14 (#56)

Breaking changes:

- The `react` library is now explicitly required in the FFI code. `purescript-react` no longer assumes that `React` is globally available. Also, with the support for 0.14 bindings of React, the DOM bindings have been split out into a separate repository [purescript-react-dom](https://github.com/purescript-contrib/purescript-react-dom).

## [v0.5.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.5.0) - 2015-11-19

- Simplified effect types for read/write access.

## [v0.4.3](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.3) - 2015-10-18

- Added `GetInitialState` argument in `spec'` (@ethul)

## [v0.4.2](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.2) - 2015-10-01

- Fixed inline styling error (@robkuz)

## [v0.4.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.1) - 2015-09-24

- Fixed a bug in `getChildren` (@ethul)

## [v0.4.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.4.0) - 2015-09-04

- Added state and props to `ReactThis` (@ethul)

## [v0.3.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.3.0) - 2015-09-01

- Updated to support React 0.13.\* (@ethul)

## [v0.2.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.2.0) - 2015-08-31

- Added additional arguments to lifecycle methods (@ethul)

## [v0.1.2](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.1.2) - 2015-08-12

- Added `displayName` (@ethul)

## [v0.1.1](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.1.1) - 2015-07-29

- Fixed `bower.json` file.

## [v0.1.0](https://github.com/purescript-contrib/purescript-react/releases/tag/v0.1.0) - 2015-07-02

Initial release.

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

- Broke up `React.DOM` module
- Made `this` reference explicit
