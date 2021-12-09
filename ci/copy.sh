#!/usr/bin/env bash

rm -rf stdlib/stdlib-fpm/test/
rm stdlib/stdlib-fpm/array3.dat

cp ci/fpm.toml stdlib/stdlib-fpm/
cp README.md stdlib/stdlib-fpm/

cp -r stdlib/stdlib-fpm/ stdlib-fpm/
cd stdlib-fpm && ls