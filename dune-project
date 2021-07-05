(lang dune 2.8)
(name cpr)
(generate_opam_files true)
(source (uri "git+ssh://git@github.com:pkel/cpr.git"))
(homepage "https://github.com/pkel/cpr")
(bug_reports "https://github.com/pkel/cpr/issues")
(license none)
(authors "Patrik Keller")
(maintainers "patrik@pkel.dev")

(package
 (name cpr)
 (synopsis "Simulating various DAG-based proof-of-work consensus protocols")
 (depends
  (ocaml (>= 4.11))
  (owl-base (>= 1.0))
  (cmdliner (>= 1.0))
  (ppx_sexp_conv (>= v0.14.1))
  ))

(package
 (name cpr-dev)
 (synopsis "Devtools")
 (depends
   (ocamlformat (= 0.15.0))
   (merlin (>= 4.2))
   (odig (>= 0.0.6))
   (odoc (>= 1.5.2))
   ))
