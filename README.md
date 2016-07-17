# hgeos

Demonstrates Haskell binding to the [GEOS][geos] [C API][capi]

Heavily inspired by [Django GEOS bindings][djangogis]

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

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[capi]: http://geos.osgeo.org/doxygen/geos__c_8h_source.html
[djangogis]: https://github.com/django/django/tree/master/django/contrib/gis/geos
[geos]: https://trac.osgeo.org/geos/
[homebrew]: http://brew.sh/
[stack]: https://haskellstack.org/
