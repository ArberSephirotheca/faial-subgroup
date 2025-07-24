(lang dune 3.17)
(cram enable)

; Generate opam files automatically
(generate_opam_files true)

; Enable automatic code formatting
(formatting
 (enabled_for ocaml))

(package
  (name faial)
  (synopsis "Static analysis tool for finding bugs in CUDA kernels")
  (description "Faial is a static analysis tool for finding bugs in CUDA kernels, featuring data-race freedom (DRF) analysis and performance analysis.")
  (authors "Tiago Cogumbreiro <cogumbreiro@gmail.com>")
  (maintainers "Tiago Cogumbreiro <cogumbreiro@gmail.com>")
  (homepage "https://gitlab.com/umb-svl/faial")
  (bug_reports "https://gitlab.com/umb-svl/faial/-/issues")
  (source (gitlab umb-svl/faial))
  (license "MIT")
  (depends
    ; Runtime dependencies
    (ocaml (>= 5.3))
    (cmdliner (>= 1.3.0))
    (ANSITerminal (>= 0.8.5))
    (otoml (>= 1.0.5))
    (yojson (>= 2.2.2))
    (printbox-text (>= 0.11))
    (fpath (>= 0.7.3))
    (z3 (>= 4.15.2))
    
    ; Build dependencies
    (dune (>= 3.17))
    
    ; Test dependencies
    (ounit (and (>= 2.2.7) :with-test))
    (alcotest (and (>= 1.9.0) :with-test))
    
    ; Documentation dependencies
    (odoc :with-doc)
    
    ; Development dependencies  
    (ocamlformat :dev)
  )
)
