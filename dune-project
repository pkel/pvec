(lang dune 3.0)

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
 (synopsis "Persistent vectors")
 (version 0.0.0)
 (depends (mtime :with-test)))

(package
 (name _dev)
 (synopsis "Dev tools")
 (depends
   (ocamlformat (>= 0.23.0))
   merlin
   odig
   odoc
   utop
   ))
