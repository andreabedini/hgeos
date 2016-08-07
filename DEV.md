# Developer guide

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

[homebrew]: http://brew.sh/
[stack]: https://haskellstack.org/
