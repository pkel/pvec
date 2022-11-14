(lang dune 3.5)

(name ocaml-vector)
(generate_opam_files true)
(source (uri "git+ssh://git@github.com:pkel/ocaml-vector.git"))
(homepage "https://github.com/pkel/ocaml-vector")
(bug_reports "https://github.com/pkel/ocaml-vector/issues")
(license none)
(authors "Patrik Keller")
(maintainers "patrik@pkel.dev")

(package
 (name vector)
 (synopsis "Clojure-style vectors for OCaml")
 (version 0.0.0))

(package
 (name _dev)
 (synopsis "Development tools & benchmark")
 (depends
   (ocamlformat (= 0.23.0))
   (merlin (>= 4.6))
   (mtime (>= 1.4.0))
   (odig (>= 0.0.8))
   (odoc (>= 2.1.1))
   (utop (>= 2.10.0))
   ))
