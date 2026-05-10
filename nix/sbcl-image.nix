# sbcl-image.nix — Pre-compiled SBCL core image for the test runner.
#
# Builds a heap snapshot (via save-lisp-and-die) with :cl-cc-test fully loaded
# and warm-stdlib-cache pre-initialized in *stdlib-vm-snapshot*.  Loading this
# core with `sbcl --core cl-cc-test.core` skips ASDF compilation entirely and
# resumes the saved Lisp state directly, cutting cold-start time from 3-5 min
# to < 60 s.
#
# Dependency chain (all content-addressed in Nix):
#   source files → ASDF system derivations → sbclWithTests → this derivation
# Touching any source file rebuilds only the changed ASDF system and its
# dependents, then rebuilds this image.
{
  pkgs,
  lib,
  sbclWithTests,
  dispatchSemFix ? null,
}:
let
  sbclBin = "${sbclWithTests}/bin/sbcl";

  # Same fileset as testAsdfSystems in asdf-systems.nix so Nix can share inputs.
  testSrc = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      (../. + "/packages")
      (../. + "/tests")
      (../. + "/cl-cc.asd")
      (../. + "/cl-cc-test.asd")
    ];
  };

  # macOS 26 ARM64: dispatch_semaphore_* regression causes GC safepoint deadlocks
  # during compilation (warm-stdlib-cache runs compile-file which triggers GC).
  # Inject the Mach-semaphore shim before starting SBCL.
  dispatchEnv = lib.optionalString (
    pkgs.stdenv.isDarwin && dispatchSemFix != null
  ) ''export DYLD_INSERT_LIBRARIES="${dispatchSemFix}/lib/libdispatch_sem_fix.dylib"'';
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "cl-cc-test-image";
  version = "0.1.0";
  src = testSrc;

  nativeBuildInputs = [
    sbclWithTests
    pkgs.coreutils
  ];

  buildPhase = ''
    runHook preBuild
    export HOME="$TMPDIR"
    ${dispatchEnv}
    # Load production systems + testing framework (NOT :cl-cc-test).
    # Excluding :cl-cc-test keeps test-file top-level forms out of the core;
    # those forms may compute paths relative to *default-pathname-defaults* at
    # load time, and we don't want Nix sandbox paths baked into the image.
    # :cl-cc-test FASLs (pre-compiled via sbclWithTests) are loaded at runtime
    # in apps.nix after *default-pathname-defaults* is reset to the user's CWD.
    ${sbclBin} --dynamic-space-size 8192 \
      --non-interactive \
      --eval '(sb-int:set-floating-point-modes :traps nil)' \
      --eval '(setf (sb-ext:bytes-consed-between-gcs) (* 2048 1024 1024))' \
      --eval '(require :asdf)' \
      --eval '(sb-ext:gc :full t)' \
      --eval '(format t "# cl-cc-test-image: loading :cl-cc~%")' \
      --eval '(handler-case (asdf:load-system :cl-cc) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))' \
      --eval '(handler-case (asdf:load-system :cl-cc-cli) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))' \
      --eval '(handler-case (asdf:load-system :cl-cc-testing-framework) (error (e) (format *error-output* "~&FATAL: ~A~%" e) (uiop:quit 1)))' \
      --eval '(format t "# cl-cc-test-image: warming stdlib cache~%")' \
      --eval '(ignore-errors (cl-cc:warm-stdlib-cache))' \
      --eval '(sb-ext:gc :full t)' \
      --eval '(setf *default-pathname-defaults* #P"/")' \
      --eval '(setf uiop:*temporary-directory* nil)' \
      --eval '(sb-ext:save-lisp-and-die "cl-cc-test.core" :executable nil)'
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp cl-cc-test.core $out/
    runHook postInstall
  '';
}
