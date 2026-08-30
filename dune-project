(lang dune 3.0)
(name lintml)
(version 0.1.0)
(generate_opam_files true)

(source (github at649/lintml))
(homepage https://github.com/at649/lintml)
(bug_reports https://github.com/at649/lintml/issues)
(license MIT)
(authors "at649")
(maintainers "at649")

(package
 (name lintml)
 (synopsis "An OCaml linter that works from the typed AST")
 (description
  "lintml reads the typed AST produced by an OCaml build and reports correctness issues, partial API use, and avoidable traversals. Style and idiom rules are available through opt-in profiles.")
 (depends
  (ocaml (and (>= 5.2) (< 5.6)))
  (dune (>= 3.0))
  (cppo (>= 1.8.0))
  (cmdliner (>= 1.2.0))
  (fmt (>= 0.9.0))
  (yojson (>= 2.0.0))
  (alcotest (and :with-test (>= 1.7.0)))
  (ocamlformat (and :with-dev-setup (= 0.29.0)))))
