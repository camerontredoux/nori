(lang dune 3.16)

(name nori)

(generate_opam_files true)

(source
 (github camerontredoux/nori))

(authors "Cameron Tredoux")

(maintainers "Cameron Tredoux")

(license LICENSE)

(package
 (name nori)
 (synopsis "Nori is a terminal based ePub downloader and reader.")
 (description "Nori scrapes Libgen's fiction section and allows users to download ePub files and read them all from the terminal.")
 (depends ocaml dune)
 (tags
  ("lambdasoup" "minttea" "riot" "tui" "reader" "epub")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
