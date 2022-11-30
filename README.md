# Persistent vectors for OCaml

`pvec` implements persistent vectors that
- like arrays, store `n` elements with keys ranging from `0` to `n-1`,
- also like arrays, support efficient random read and write access, but
- unlike arrays, are persistent/immutable and resize dynamically.

Until recently I tried using lists until it stopped making sense, then
transitioned to arrays where I had to. The mutable nature of arrays
often introduced bugs. In some sense, I was trading speed for safety.

With persistent vectors I can have the best of both worlds. In many
cases they strike a good balance between safety and speed.

## Documentation

The `pvec` library exposes a single module `Pvec`. You can explore its
interface online.
- https://pkel.github.io/pvec/pvec/Pvec/index.html

This documentation is derived from the `main` branch on Github. You
might have obtained a different version of the library from Opam.

If you're interested in the underlying data-structure, I recommend
reading [hyPiRion's series of blog
posts](https://hypirion.com/musings/understanding-persistent-vector-pt-1).

## License

This library is distributed under the ISC license. See
[`./LICENSE`](https://github.com/pkel/pvec/blob/main/LICENSE).

## Progress
- [x] base implementation
- [x] test random updates & persistence
- [x] basic documentation
- [x] setup CI
- [x] provide a couple of convenience functions (fold, map)
- [x] write intro to `pvec.mli` and readme
- [x] pick license
- [x] release
- [ ] more benchmarks
- [ ] slice/range support for seq, copy, map & fold
