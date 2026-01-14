(lang dune 3.17)
(cram enable)
(using menhir 2.0)
; Generate opam files automatically
(generate_opam_files true)

; Enable automatic code formatting
(formatting
 (enabled_for ocaml))

(package
  (name faial)
  (synopsis "Static analysis tool for finding bugs in CUDA kernels")
  (description "Faial is a static analysis tool for finding bugs in CUDA kernels, featuring data-race freedom (DRF) analysis and performance analysis.")
  (authors "Tiago Cogumbreiro <cogumbreiro@users.sf.net>")
  (maintainers "Tiago Cogumbreiro <cogumbreiro@users.sf.net>")
  (homepage "https://gitlab.com/umb-svl/faial")
  (bug_reports "https://gitlab.com/umb-svl/faial/-/issues")
  (source (gitlab umb-svl/faial))
  (license "MIT")
  (depends
    ; Runtime dependencies
    (ocaml (>= 5.3.0))
    (cmdliner (>= 2.1.0))
    (ANSITerminal (>= 0.8.5))
    (otoml (>= 1.0.5))
    (yojson (>= 3.0.0))
    (printbox-text (>= 0.12))
    (fpath (>= 0.7.3))
    (z3 (>= 4.15.2))
    (menhir (>= 20250912))
    
    ; Build dependencies
    (dune (>= 3.17))
    
    ; Test dependencies
    (ounit (and (>= 2.2.7) :with-test))
    (alcotest (and (>= 1.9.0) :with-test))

    ; Development dependencies
    (ocamlformat :dev)
  )
)
