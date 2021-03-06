# dp-stdlib

[![MIT](https://img.shields.io/github/license/zoziha/dp-stdlib?color=pink)](LICENSE)
[![fpm](https://github.com/zoziha/dp-stdlib/workflows/fpm/badge.svg)](https://github.com/zoziha/dp-stdlib/actions)

`dp-stdlib` is a downstream branch of [Fortran standard library](https://github.com/fortran-lang/stdlib) 
and a [fpm](https://github.com/fortran-lang/fpm) package, using `real(real64)` 
and `integer(int32)`, adapts to the lightweight compilation pressure.

## Build with Fortran-lang/fpm

Fortran Package Manager (fpm) is a package manager and build system for Fortran. <br>
You can build `dp-stdlib` using the provided `fpm.toml`:

```sh
fpm build --profile release
```

To use `dp-stdlib` within your `fpm` project, add the following lines to your `fpm.toml` file:

```toml
[dependencies]
dp-stdlib = { git="https://github.com/zoziha/dp-stdlib" }
```

## How to contribute

```sh
git checkout ci
# and edit the CI, source files
```

## Links

- [fortran-lang website](https://fortran-lang.org/)
- [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
- [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)