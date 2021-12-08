# dp-stdlib

`dp-stdlib` is a downstream branch of Fortran standard library and a fpm package, using `real(real64)` 
and `integer(int32`, adapts to the lightweight compilation pressure.

## Build with Fortran-lang/fpm

Fortran Package Manager (FPM) is a package manager and build system for Fortran. <br>
You can build `dp-stdlib` using the provided `fpm.toml`:

```sh
fpm build --profile release
```

To use `dp-stdlib` within your `fpm` project, add the following lines to your `fpm.toml` file:

```toml
[dependencies]
dp-stdlib = { git="https://github.com/zoziha/dp-stdlib" }
```

## Links

- [fortran-lang website](https://fortran-lang.org/)
- [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
- [fortran/lang/stdlib](https://github.com/fortran-lang/stdlib)