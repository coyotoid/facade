(lang dune 3.21)

(name facade)

(generate_opam_files true)

(source
 (codeberg lobo/shape))

(authors "Javier B. Torres <lobo@quiltro.org>")

(maintainers "Javier B. Torres <lobo@quiltro.org>")

(license MIT)

(package
 (name facade)
 (synopsis "A library for serialization in OCaml")
 (depends
  ocaml))

(package
 (name facade-yojson)
 (synopsis "Yojson backend for facade")
 (depends
  shape
  (yojson (>= "3.0.0"))
  (ounit2 :with-test)))

(package
 (name facade-msgpck)
 (synopsis "MessagePack backend for facade")
 (depends
  facade
  (msgpck (>= "1.7"))
  (ounit2 :with-test)))
