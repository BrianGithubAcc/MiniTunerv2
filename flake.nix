{
  description = "Development environment for MiniTuner";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.satire = {
    url = "github:arnabd88/Satire/v1.1";
    flake = false;
  };

  outputs = { self, nixpkgs, satire }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          # FPTaylor still uses Pervasives, which was removed in OCaml 5.x.
          ocaml = pkgs.ocaml-ng.ocamlPackages_4_14;
          # Current nixpkgs packages NumPy >=2.5, which requires Python 3.12.
          # SATIRE v1.1 is source-compatible with 3.12 after its SLY/SymEngine
          # dependencies are supplied by this environment.
          python = pkgs.python312.withPackages (ps: with ps; [
            gmpy2
            lark
            matplotlib
            numpy
            pandas
            sly
            sympy
            symengine
            z3-solver
          ]);
          setup = pkgs.writeShellScriptBin "minituner-setup" ''
            set -eu
            root="''${MINITUNER_ROOT:-$PWD}"

            satire_dir="$root/.deps/satire"
            satire_revision="dda63f321a905f8be51e3648427bafea42352d40"
            if [ ! -f "$satire_dir/.minituner-revision" ] || \
               [ "$(cat "$satire_dir/.minituner-revision")" != "$satire_revision" ]; then
              rm -rf "$satire_dir"
              mkdir -p "$satire_dir"
              cp -R ${satire}/. "$satire_dir/"
              chmod -R u+w "$satire_dir"
              printf '%s\n' "$satire_revision" > "$satire_dir/.minituner-revision"
            fi
            if ! grep -q 'set_custom_noise' "$satire_dir/src/ASTtypes.py"; then
              patch -d "$satire_dir" -p1 < "$root/patches/satire-custom-noise.patch"
            fi

            if ! grep -q 'MINITUNER_GELPIA_SERIAL' "$root/FPTaylor/opt_gelpia.ml"; then
              patch -d "$root/FPTaylor" -p1 < "$root/patches/fptaylor-gelpia-serial.patch"
            fi

            fptaylor_fingerprint="$(
              sha256sum \
                "$root/FPTaylor/Makefile" \
                "$root/FPTaylor/opt_gelpia.ml" \
                "$root/FPTaylor/INTERVAL/chcw.c" \
                "$root/patches/fptaylor-glibc.patch" \
                "$root/patches/fptaylor-gelpia-serial.patch" |
                sha256sum | cut -d' ' -f1
            )"
            fptaylor_stamp="$root/.deps/fptaylor-build.fingerprint"
            if [ ! -x "$root/FPTaylor/fptaylor" ] || \
               [ ! -f "$fptaylor_stamp" ] || \
               [ "$(cat "$fptaylor_stamp")" != "$fptaylor_fingerprint" ]; then
              fptaylor_patched=0
              restore_fptaylor_source() {
                if [ "$fptaylor_patched" -eq 1 ]; then
                  patch -R -d "$root/FPTaylor" -p1 < "$root/patches/fptaylor-glibc.patch"
                fi
              }
              trap restore_fptaylor_source EXIT
              if ! grep -q 'double mt_fadd' "$root/FPTaylor/INTERVAL/chcw.c"; then
                patch -d "$root/FPTaylor" -p1 < "$root/patches/fptaylor-glibc.patch"
                fptaylor_patched=1
              fi

              echo "Building FPTaylor with OCaml $(ocamlc -version)..."
              make -C "$root/FPTaylor" clean-all
              make -C "$root/FPTaylor" \
                ML="ocamlfind ocamlc -package num" \
                OPT_ML="ocamlfind ocamlopt -package num" \
                all
              restore_fptaylor_source
              fptaylor_patched=0
              trap - EXIT
              printf '%s\n' "$fptaylor_fingerprint" > "$fptaylor_stamp"
            else
              echo "FPTaylor build is current."
            fi

            # FPTaylor can import Gelpia in freshly spawned Python workers
            # before Gelpia's CLI has initialised its logging globals.
            if grep -q '^LOG_LEVEL = None$' "$root/gelpia/bin/gelpia_logging.py"; then
              patch -d "$root/gelpia" -p1 < "$root/patches/gelpia-python-logging.patch"
            fi

            # The repository contains prebuilt Gelpia tools. Point their ELF
            # loader and RPATH at the pinned Nix runtime and bundled libraries.
            gelpia_rpath="${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib ]}:$root/gelpia/requirements/lib:$root/gelpia/target/release"
            for executable in \
              "$root/gelpia/bin/gaol_repl" \
              "$root/gelpia/target/release/serial" \
              "$root/gelpia/target/release/cooperative" \
              "$root/gelpia/target/release/cooperative-mt" \
              "$root/gelpia/requirements/bin/cargo" \
              "$root/gelpia/requirements/bin/rustc"
            do
              if [ -f "$executable" ]; then
                patchelf --set-interpreter "${pkgs.stdenv.cc.bintools.dynamicLinker}" \
                  --set-rpath "$gelpia_rpath" "$executable"
              fi
            done

            echo "MiniTuner dependencies are ready."
          '';
        in {
          default = pkgs.mkShell {
            packages = [
              python
              setup

              ocaml.ocaml
              ocaml.dune_3
              ocaml.findlib
              ocaml.menhir
              ocaml.num
              ocaml.yojson
              ocaml.z3
              ocaml.zarith

              pkgs.bison
              pkgs.boost
              pkgs.cargo
              pkgs.cmake
              pkgs.flex
              pkgs.gappa
              pkgs.gawk
              pkgs.gcc
              pkgs.git
              pkgs.gmp
              pkgs.gnumake
              pkgs.libffi
              pkgs.mpfr
              pkgs.patchelf
              pkgs.patch
              pkgs.pkg-config
              pkgs.rustc
              pkgs.scons
              pkgs.sollya
            ];

            shellHook = ''
              export MINITUNER_ROOT="$PWD"
              export MINITUNER_PYTHON="${python}/bin/python3"
              export FPTAYLOR_BASE="$PWD/FPTaylor"
              export GELPIA_PATH="$PWD/gelpia"
              export SATIRE_PATH="$PWD/.deps/satire"
              export PYTHONPATH="$PWD/gelpia:$PWD/gelpia/bin''${PYTHONPATH:+:$PYTHONPATH}"
              export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib ]}:$PWD/gelpia/requirements/lib:$PWD/gelpia/target/release''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              export LIBRARY_PATH="$PWD/gelpia/requirements/lib''${LIBRARY_PATH:+:$LIBRARY_PATH}"
              export CPLUS_INCLUDE_PATH="$PWD/gelpia/requirements/include''${CPLUS_INCLUDE_PATH:+:$CPLUS_INCLUDE_PATH}"
              export CAML_LD_LIBRARY_PATH="$(ocamlfind query num)''${CAML_LD_LIBRARY_PATH:+:$CAML_LD_LIBRARY_PATH}"
              export PATH="$PWD/FPTaylor:$PWD/gelpia/bin:$PATH"

              echo "MiniTuner development shell"
              echo "  Setup:  minituner-setup  # first time only"
              echo "  Build:  cd src && dune build"
              echo "  Run:    cd src && dune exec ./minitune.exe -- -e 'x in [0.0078125,0.5];(exp(x) - 1)/x'"
              echo "  Suite:  ./run_all_fpcore.sh --help"
              echo "  Native: ./run_minitune.sh 'x in [0,1];exp(x)' demo"
              echo "  Compat: ./run_all_fpcore.sh --optuner-compatible --search-engine auto"
              echo "  Fast:   ./run_all_fpcore.sh --optuner-satire-compatible --search-engine auto"
            '';
          };
        });
    };
}
