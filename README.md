# hgeos [![Hackage](https://img.shields.io/hackage/v/hgeos.svg?maxAge=2592000)](http://hackage.haskell.org/package/hgeos) [![Travis](https://travis-ci.org/rcook/hgeos.svg)](https://travis-ci.org/rcook/hgeos) [![GitHub issues](https://img.shields.io/github/issues/rcook/hgeos.svg)](https://github.com/rcook/hgeos/issues) [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rcook/hgeos/master/LICENSE)

Simple Haskell bindings to the [Geometry Engine Open Source][geos]
[C API][capi] heavily inspired by [Django GEOS bindings][django-gis]

Documentation is available on Hackage:

* [`Data.Geolocation.GEOS`][data-geolocation-geos]
* [`Data.Geolocation.GEOS.Imports`][data-geolocation-geos-imports]
* [`Data.Geolocation.GEOS.Trans`][data-geolocation-geos-trans]

Note that the bindings are very far from complete. There's just about enough of
the API covered to compute intersections of polygons, which is the bare minimum
of the API required to render [my viz][carnivore-tracker-viz]. I will add to
the API as and when I need specific GEOS functionality. If there are specific
portions that you would like implemented, please open a [ticket][issues] or
submit a [pull request][pull-requests] for it.

## Prerequisites

This project currently links dynamically to your global installation of GEOS
which you'll need to install before you can build the code. Future versions may
bake GEOS in directly.

### Mac OS X

Using [Homebrew][homebrew]:

```bash
$ brew install geos
```

### Ubuntu

```bash
$ sudo apt-get install libgeos++-dev
```

### Windows

* Windows binaries are included in the repository and resulting package

## Development

This project uses [Stack][stack].

### Configure

```bash
$ stack setup
```

### Clean

```bash
$ stack clean
```

### Build

```bash
$ stack build
```

### Test

```bash
$ stack test
```

### Build with C function tracing enabled

```bash
$ stack build --ghc-options -optc-DENABLE_TRACE
```

### Run containerized Cabal tests

Create Docker image:

```bash
$ script/build-cabal-image
```

Run Cabal tests:

```bash
$ script/run-cabal-test
```

Run Cabal shell for debugging:

```bash
$ script/run-cabal-shell
```

### Build and run all tests

```bash
$ script/test
```

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[capi]: http://geos.osgeo.org/doxygen/geos__c_8h_source.html
[carnivore-tracker-viz]: https://public.tableau.com/profile/richard.cook#!/vizhome/CarnivoreTrackerTheNextGeneration/Heatmap
[data-geolocation-geos]: http://hackage.haskell.org/package/hgeos/docs/Data-Geolocation-GEOS.html
[data-geolocation-geos-imports]: http://hackage.haskell.org/package/hgeos/docs/Data-Geolocation-GEOS-Imports.html
[data-geolocation-geos-trans]: http://hackage.haskell.org/package/hgeos/docs/Data-Geolocation-GEOS-Trans.html
[django-gis]: https://github.com/django/django/tree/master/django/contrib/gis/geos
[geos]: https://trac.osgeo.org/geos/
[homebrew]: http://brew.sh/
[issues]: https://github.com/rcook/hgeos/issues
[pull-requests]: https://github.com/rcook/hgeos/pulls
[stack]: https://haskellstack.org/
