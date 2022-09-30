(lang dune 3.0)
(generate_opam_files true)

(source (github maiste/terminus-async))
(license MIT)
(authors
  "Étienne Marais <etienne@maiste.fr>")
(maintainers
  "Étienne Marais <etienne@maiste.fr>")
(documentation "maiste.github.io/terminus-async")

(package
  (name terminus-async)
  (synopsis "Terminus with the cohttp-async request handler")
  (description
    "Terminus-async is an implementation of the Terminus specification using cohttp-async")
  (depends
    (terminus (= :version))
    (ocaml (< 5.0.0))
    cohttp-async
    (httpaf-async :with-test)
    (alcotest :with-test)
    (alcotest-async :with-test)
    (odoc :with-doc))
  (conflicts
    (result (< 1.5))))

