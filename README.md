# Persistent vectors for OCaml

The `pvec` library exposes a single module `Pvec`. You can explore its
interface online.
- https://pkel.github.io/pvec/pvec/Pvec/index.html

The documentation is derived from the `main` branch. Opam might provide
different versions.

Background info is provided elsewhere:
- https://hypirion.com/musings/understanding-persistent-vector-pt-1

## Progress
- [x] base implementation
- [x] test random updates & persistence
- [x] basic documentation
- [x] setup CI
- [x] provide a couple of convenience functions (fold, map)
- [ ] write intro to `pvec.mli`, also include in this readme
- [ ] pick license and release
- [ ] more benchmarks
- [ ] slice/range support for seq, copy, map & fold
