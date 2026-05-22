{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  name = "facade";
  packages = [
    pkgs.entr
    pkgs.bash
    pkgs.python3
    pkgs.ocamlPackages.ocaml
    pkgs.ocamlPackages.dune_3
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.ocaml-lsp
    pkgs.ocamlPackages.ocamlformat
    pkgs.ocamlPackages.odoc
    pkgs.ocamlPackages.merlin
    pkgs.ocamlPackages.ocp-indent
    pkgs.ocamlPackages.yojson
    pkgs.ocamlPackages.msgpck
    pkgs.ocamlPackages.ounit2
  ];
}
