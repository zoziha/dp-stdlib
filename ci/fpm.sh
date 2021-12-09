#!/usr/bin/env bash

ls
cp src/**.fypp stdlib/src/

cd stdlib
bash ./ci/fpm-deployment.sh