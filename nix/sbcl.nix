{ pkgs }:
# macOS 26.4.1 ARM64 has four SBCL workarounds bundled together here:
#
# 1. libdispatch regression: dispatch_semaphore_signal fails to wake
#    dispatch_semaphore_wait. SBCL uses dispatch_semaphore for its safepoint
#    stop-world mechanism (LISP_FEATURE_SB_SAFEPOINT, all GC types).
#    Fix: see ./dispatch-sem-fix.nix — replaces dispatch_semaphore_* with
#    Mach semaphores via DYLD_INSERT_LIBRARIES.
#
# 2. libsystem_m regression: log(0.0) spins indefinitely due to a SIGFPE
#    handler bug. Fix: emit `(sb-int:set-floating-point-modes :traps nil)`
#    at SBCL startup (handled by apps.nix bootstrap).
#
# 3. markRegionGC = false: SBCL's mark-region GC interacts badly with
#    macOS 26's libdispatch; disable it so we use the classic GC.
#
# 4. bytes-consed-between-gcs = 2GB: With the dispatchSemFix shim and
#    classic GC, SBCL allocates aggressively; raising the GC trigger
#    keeps our test suite from thrashing collection during compile.
{
  sbcl =
    if pkgs.stdenv.isDarwin then
      pkgs.wrapLisp {
        pkg = pkgs.sbcl.overrideAttrs (_: {
          markRegionGC = false;
        });
        faslExt = "fasl";
        flags = [
          "--dynamic-space-size"
          "3000"
        ];
      }
    else
      pkgs.sbcl;

  # MANDATORY bootstrap eval-chain (order matters), implementing macOS workarounds
  # 2 (set-floating-point-modes silences libsystem_m log(0.0) SIGFPE handler bug)
  # and 4 (bytes-consed-between-gcs raises GC trigger to 2GB). require :asdf must
  # come AFTER any sb-cover declaim in lispPreLoadEvalForms; gc :full reclaims
  # bootstrap conses and is intentionally last in the chain.
  sbclBootstrap = ''
    --eval '(sb-int:set-floating-point-modes :traps nil)' \
    --eval '(setf (sb-ext:bytes-consed-between-gcs) (* 2048 1024 1024))' \
    --eval '(require :asdf)' \
    --eval '(sb-ext:gc :full t)'
  '';
}
