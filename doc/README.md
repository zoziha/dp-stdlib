# How-to

ENV: ubuntu-latest, gcc 10

1. Checkout `dp-stdlib` and `stdlib`;
2. Setup pyhton, fypp, gfortran and fpm;
3. Run `dp-stdlib/ci/fpm.sh` of `dp-stdlib`;
   - cp `dp-stdlib/ci/**.fypp` to `stdlib/src/`.
4. Run `stdlib/ci/fpm-deployment.sh` of `stdlib`;
5. Deploy `dp-stdlib`.