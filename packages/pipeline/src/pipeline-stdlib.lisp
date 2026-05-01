;;;; packages/pipeline/src/pipeline-stdlib.lisp — Standard Library Cache Infrastructure
;;;
;;; Extracted from pipeline.lisp.
;;; Contains the stdlib expanded-form cache: snapshot/restore helpers,
;;; cache build, and get-stdlib-forms entry point.
;;;
;;; Why a separate file: the cache subsystem is a self-contained unit
;;; with its own rollback logic; keeping it separate lets pipeline.lisp
;;; focus on compile-expression, compile-string, and run-string.
;;;
;;; Depends on pipeline.lisp (compile-expression is in the same module,
;;; but this file loads BEFORE pipeline.lisp).  Actually depends on
;;; stdlib-source{,-ext}.lisp (*standard-library-source*),
;;; expand/macro.lisp (*macro-eval-fn*, compiler-macroexpand-all),
;;; and parse/cl/* (parse-all-forms).
;;; Load order: before pipeline.lisp.

(in-package :cl-cc/pipeline)

;;; ─── Stdlib Expanded-Form Cache ──────────────────────────────────────────

(defparameter *stdlib-expanded-cache* nil
  "Cached list of stdlib forms after parse + macro-expand, but BEFORE
`lower-sexp-to-ast'.  Entries are s-expressions (or occasionally
already-lowered AST nodes, whenever `compiler-macroexpand-all' short-circuits
because a form was handed in pre-lowered).

Rationale for caching at the sexp level rather than post-lower AST:
AST nodes carry identity-sensitive state (gensym registers, slot writers
via defstruct, label counters introduced during lowering) that must NOT
be shared across independent compiler contexts.  The previous post-lower
cache broke tests like OR-UNIQUE-GENSYM-PER-EXPANSION because the same
AST node was handed to multiple compilations, yielding colliding gensyms.
Sexps, by contrast, are plain reader data and are naturally immutable
when treated as read-only — every call to `get-stdlib-forms' produces a
fresh AST tree downstream via the standard lowering pipeline.

Reused across every `:stdlib t' compile so the ~857-line stdlib is not
re-parsed or re-expanded for every test.  The downstream
`lower-sexp-to-ast' and `compile-ast' passes still run per-call, which
is the semantically correct behaviour for a per-test compiler context.

Cache is keyed jointly by *standard-library-source* identity AND the
currently active *macro-eval-fn*, because macro expansion side effects
depend on which evaluator ran them; rebinding *macro-eval-fn* transparently
invalidates the cache.")

(defparameter *stdlib-expanded-cache-source* nil
  "Source-string object used to populate *stdlib-expanded-cache*.
Compared by EQ to detect when the source has been rebound.")

(defparameter *stdlib-expanded-cache-eval-fn* nil
  "*macro-eval-fn* value active when *stdlib-expanded-cache* was populated.
Compared by EQ to detect when the evaluator has been swapped
(e.g. self-hosting bootstrap flipping from host eval to our-eval).")

(defparameter *stdlib-vm-snapshot* nil
  "vm-io-state after compiling and executing *standard-library-source*.
Cloned per run-string call that uses :stdlib t so each test gets an
isolated VM with stdlib functions pre-loaded.")

(defparameter *stdlib-accessor-slot-map* nil
  "Snapshot of *accessor-slot-map* after stdlib compilation.
Copied into each run-string :stdlib t call so defstruct accessors
defined by the stdlib are available during user-form compilation.")

(defparameter *stdlib-defstruct-slot-registry* nil
  "Snapshot of *defstruct-slot-registry* after stdlib compilation.
Copied into each run-string :stdlib t call so struct slot inheritance
defined by the stdlib is available during user-form compilation.")

(defun %snapshot-macro-env-table ()
  "Return a shallow copy of the current macro environment hash table.
Used by `%build-stdlib-expanded-cache' to support rollback on non-local exit
during macro expansion.

SCOPE: snapshots ONLY the `macro-env-table' hash (name → expander).
Does NOT cover `*compiler-macro-table*' or any macro-expansion memo
caches.  A stdlib that calls `define-compiler-macro' and then throws
mid-build will leave its compiler-macro entries behind.  Acceptable
today because stdlib does not register compiler-macros; revisit if
that changes."
  (let* ((src (macro-env-table *macro-environment*))
         (dst (make-hash-table :test (hash-table-test src)
                               :size (hash-table-count src))))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    dst))

(defun %restore-macro-env-table (snapshot)
  "Replace the macro environment table contents with SNAPSHOT atomically
from the caller's perspective.

Strategy: build a FRESH replacement hash populated from SNAPSHOT first,
THEN overwrite the live table's contents in one `clrhash'+`maphash' pair.
If the fresh-build step throws, the live table is untouched (original
torn state still observable, but no worse than pre-call).  The previous
naive `clrhash'+re-populate approach would leave the live table EMPTY if
`maphash' re-population threw (e.g., OOM), which is strictly worse than
the torn state we were trying to recover from.

Object identity of `*macro-environment*' and its backing hash table is
preserved, so any code holding a reference sees the rollback."
  (let* ((live (macro-env-table *macro-environment*))
         (fresh (make-hash-table :test (hash-table-test live)
                                 :size (hash-table-count snapshot))))
    ;; Phase 1: build replacement completely. If this throws, live is untouched.
    (maphash (lambda (k v) (setf (gethash k fresh) v)) snapshot)
    ;; Phase 2: commit. clrhash + maphash on a pre-sized table is near-atomic.
    (clrhash live)
    (maphash (lambda (k v) (setf (gethash k live) v)) fresh)))

(defun %build-stdlib-expanded-cache ()
  "Parse and macro-expand *standard-library-source* into a list of forms.
Returns a list of sexps (plain reader data, safely shareable).

Side effect on success: any `defmacro' in the stdlib is installed in the
global macro registry during macro-expansion.  This is the intended
behaviour — later cache hits rely on those macros being globally
available when user forms are expanded.

On NON-LOCAL EXIT (error / throw / unwind) mid-build, we restore the
pre-build macro registry snapshot so the runtime does not end up with a
torn, partially-installed set of stdlib macros that would poison later
compilation attempts (concrete failure: user compile would hit
`undefined macro FOO' for a stdlib macro whose registration was rolled
back mid-sequence but whose dependents remained registered).  Normal
completion intentionally leaves the snapshot-less state in place — the
registry is populated and stays so.

INVARIANT: returned list contains ONLY sexps, never AST nodes.  The
previous cache allowed AST nodes (for paths where `compiler-macroexpand-all'
short-circuited on pre-lowered input) but shared AST nodes carry
identity-sensitive gensym registers and re-introduce the
OR-UNIQUE-GENSYM-PER-EXPANSION regression.  We assert this explicitly
below as a tripwire against silent re-introduction of that bug."
  (let ((snapshot (%snapshot-macro-env-table))
        (success-p nil))
    (unwind-protect
         (let ((result (mapcar #'compiler-macroexpand-all
                               (parse-all-forms *standard-library-source*))))
           ;; Tripwire: stdlib source is plain text; every entry MUST be a sexp.
           (dolist (form result)
             (when (typep form 'ast-node)
               (error "stdlib cache builder produced an AST node; only sexps allowed (would re-introduce gensym sharing bug)")))
           (setf success-p t)
           result)
      (unless success-p
        (%restore-macro-env-table snapshot)))))

(defun get-stdlib-forms ()
  "Return stdlib forms (sexps post-expand) ready to hand to
`compile-toplevel-forms'.  On the first call — or whenever
*standard-library-source* or *macro-eval-fn* has been rebound — this
rebuilds the cache.  Subsequent calls are O(1) plus one copy-tree.

The returned tree is fresh (via copy-tree) so downstream code cannot corrupt
the shared cache by mutating nested cons structure. This matters under
parallel test execution because macro/lowering paths may destructure and
reuse sublists while compiling stdlib-heavy forms."
  (unless (and (eq *stdlib-expanded-cache-source*  *standard-library-source*)
               (eq *stdlib-expanded-cache-eval-fn* *macro-eval-fn*))
    ;; Snapshot the key values BEFORE running the build.  If macro
    ;; expansion during build flips `*macro-eval-fn*' (the self-host
    ;; bootstrap does exactly this), we must key the cache by the fn
    ;; that was active at build ENTRY, not by whatever survives after
    ;; `mapcar' completes.
    (let ((src-at-entry     *standard-library-source*)
          (eval-fn-at-entry *macro-eval-fn*))
      (setf *stdlib-vm-snapshot* nil)
      (setf *stdlib-expanded-cache*         (%build-stdlib-expanded-cache)
            *stdlib-expanded-cache-source*  src-at-entry
            *stdlib-expanded-cache-eval-fn* eval-fn-at-entry)))
  (copy-tree *stdlib-expanded-cache*))
