(lang dune 3.5)

(name pvec)
(generate_opam_files true)
(source (uri "git+ssh://git@github.com:pkel/pvec.git"))
(homepage "https://github.com/pkel/pvec")
(bug_reports "https://github.com/pkel/pvec/issues")
(license none)
(authors "Patrik Keller")
(maintainers "patrik@pkel.dev")

(package
 (name pvec)
 (synopsis "Persistent vectors for OCaml")
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
