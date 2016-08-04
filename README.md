# hgeos [![Hackage](https://img.shields.io/hackage/v/hgeos.svg?maxAge=2592000)](http://hackage.haskell.org/package/hgeos)

Simple Haskell bindings to the [GEOS][geos] [C API][capi] heavily inspired by
[Django GEOS bindings][django-gis]

Documentation is available on Hackage:

* [`Data.Geolocation.GEOS`][data-geolocation-geos]
* [`Data.Geolocation.GEOS.Imports`][data-geolocation-geos-imports]

## Development

This project uses [Stack][stack].

## Prerequisites

This project currently links dynamically to your global installation
of GEOS which you'll need to install before you can build the code.
Future versions may bake GEOS in directly.

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

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[capi]: http://geos.osgeo.org/doxygen/geos__c_8h_source.html
[data-geolocation-geos]: http://hackage.haskell.org/package/hgeos/docs/Data-Geolocation-GEOS.html
[data-geolocation-geos-imports]: http://hackage.haskell.org/package/hgeos/docs/Data-Geolocation-GEOS-Imports.html
[django-gis]: https://github.com/django/django/tree/master/django/contrib/gis/geos
[geos]: https://trac.osgeo.org/geos/
[homebrew]: http://brew.sh/
[stack]: https://haskellstack.org/
