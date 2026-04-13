;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc)

(defun compile-expression (expr &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  (let* ((ctx (make-instance 'compiler-context :safety safety))
          (expanded-expr (if (typep expr 'ast-node)
                             expr
                             (compiler-macroexpand-all expr)))
          (ast (if (typep expanded-expr 'ast-node)
                   expanded-expr
                   (lower-sexp-to-ast expanded-expr)))
          (optimized-ast (optimize-ast ast))
          (inferred-type (when type-check
                           (handler-case
                               (type-check-ast optimized-ast)
                             (error (e)
                               (if (eq type-check :strict)
                                   (error e)
                                   (progn
                                     (warn "Type check warning: ~A" e)
                                     nil))))))
          (result-reg (compile-ast optimized-ast ctx))
          (instructions (nreverse (ctx-instructions ctx)))
          (full-instructions (append instructions
                                     (list (make-vm-halt
                                            :reg result-reg))))
          (optimized-instructions nil)
          (leaf-p nil)
          (optimized-program nil))
      (multiple-value-setq (optimized-instructions leaf-p)
        (optimize-instructions full-instructions
                               :pass-pipeline pass-pipeline
                               :print-pass-timings print-pass-timings
                               :timing-stream timing-stream
                               :print-pass-stats print-pass-stats
                               :stats-stream stats-stream
                               :trace-json-stream trace-json-stream
                               :print-opt-remarks print-opt-remarks
                               :opt-remarks-stream opt-remarks-stream
                               :opt-remarks-mode opt-remarks-mode))
      (setf optimized-program (make-vm-program
                              :instructions full-instructions
                              :result-register result-reg
                              :leaf-p leaf-p))
     ;; Capture label counter for REPL continuity
     (when *repl-capture-label-counter*
       (setf *repl-capture-label-counter* (ctx-next-label ctx)))
     (make-compilation-result :program optimized-program
                                :assembly (emit-assembly optimized-program :target target)
                                :type (when type-check inferred-type)
                                :type-env (ctx-type-env ctx)
                                :cps (maybe-cps-transform optimized-ast)
                                :ast optimized-ast
                                :vm-instructions full-instructions
                                :optimized-instructions optimized-instructions)))

;;; Standard Library (Higher-Order Functions)
;;; *standard-library-source* is defined in stdlib-source.lisp (loaded before this file).

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
           ;; If this ever fires, someone fed pre-lowered AST into the cache path
           ;; and the gensym-sharing bug is about to resurface.
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
rebuilds the cache.  Subsequent calls are O(1) plus one copy-list.

The returned spine is fresh (via copy-list) so downstream code walking
destructively cannot corrupt the cache.  The form contents are sexps
and therefore safely shared: `lower-sexp-to-ast' inside
`compile-toplevel-forms' produces fresh AST nodes on every call."
  (unless (and (eq *stdlib-expanded-cache-source*  *standard-library-source*)
               (eq *stdlib-expanded-cache-eval-fn* *macro-eval-fn*))
    ;; Snapshot the key values BEFORE running the build.  If macro
    ;; expansion during build flips `*macro-eval-fn*' (the self-host
    ;; bootstrap does exactly this), we must key the cache by the fn
    ;; that was active at build ENTRY, not by whatever survives after
    ;; `mapcar' completes.  Otherwise a mid-build flip would result in
    ;; a cache stored under the post-flip fn but populated from
    ;; pre-flip expansion — silently stale for every subsequent hit.
    (let ((src-at-entry     *standard-library-source*)
          (eval-fn-at-entry *macro-eval-fn*))
      (setf *stdlib-expanded-cache*         (%build-stdlib-expanded-cache)
            *stdlib-expanded-cache-source*  src-at-entry
            *stdlib-expanded-cache-eval-fn* eval-fn-at-entry)))
  (copy-list *stdlib-expanded-cache*))

(defun warm-stdlib-cache ()
  "Populate the stdlib expanded-form cache if it has not been built yet.
Intended to be called once from the test runner BEFORE spawning parallel
worker threads, so that:
  (a) no worker pays the cold-miss cost (~full stdlib parse + expand)
      inside the 10-second per-test budget, and
  (b) multiple workers cannot race on the first miss, which would
      otherwise cause duplicate macro registration and torn cache
      reads between *stdlib-expanded-cache* and its key cells."
  (get-stdlib-forms)
  (values))

(defun parse-source-for-language (source language)
  "Parse SOURCE according to LANGUAGE, returning a list of AST nodes or s-expressions.
:LISP returns s-expressions (compile-toplevel-forms handles lowering).
:PHP calls parse-php-source which returns AST nodes directly."
  (case language
    (:lisp (parse-all-forms source))
    (:php (parse-php-source source))
    (t (error "Unknown language: ~S" language))))

(defun compile-string (source &key (target :x86_64) type-check (language :lisp) (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  (let ((forms (parse-source-for-language source language)))
    (if (and (eq language :lisp) (= (length forms) 1))
        (compile-expression (first forms)
                            :target target :type-check type-check :safety safety
                            :pass-pipeline pass-pipeline
                             :print-pass-timings print-pass-timings
                             :timing-stream timing-stream
                             :print-pass-stats print-pass-stats
                             :stats-stream stats-stream
                             :trace-json-stream trace-json-stream
                             :print-opt-remarks print-opt-remarks
                            :opt-remarks-stream opt-remarks-stream
                            :opt-remarks-mode opt-remarks-mode)
        ;; Multiple forms (or non-lisp): use compile-toplevel-forms for sequential macro expansion
        (compile-toplevel-forms forms
                                :target target :type-check type-check :safety safety
                                :pass-pipeline pass-pipeline
                                :print-pass-timings print-pass-timings
                                :timing-stream timing-stream
                                :print-pass-stats print-pass-stats
                                :stats-stream stats-stream
                                :trace-json-stream trace-json-stream
                                :print-opt-remarks print-opt-remarks
                                :opt-remarks-stream opt-remarks-stream
                                :opt-remarks-mode opt-remarks-mode))))

(defun run-string (source &key stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile and run SOURCE. When STDLIB is true, include standard library."
  (let* ((*package* (find-package :cl-cc))
         (*accessor-slot-map* (make-hash-table :test #'eq))
         (*defstruct-slot-registry* (make-hash-table :test #'eq))
         (*labels-boxed-fns* nil)
         (result (if stdlib
                     (compile-string-with-stdlib source :target :vm
                                                  :pass-pipeline pass-pipeline
                                                  :print-pass-timings print-pass-timings
                                                   :timing-stream timing-stream
                                                   :print-pass-stats print-pass-stats
                                                   :stats-stream stats-stream
                                                   :trace-json-stream trace-json-stream
                                                   :print-opt-remarks print-opt-remarks
                                                  :opt-remarks-stream opt-remarks-stream
                                                  :opt-remarks-mode opt-remarks-mode)
                     (compile-string source :target :vm
                                      :pass-pipeline pass-pipeline
                                      :print-pass-timings print-pass-timings
                                       :timing-stream timing-stream
                                       :print-pass-stats print-pass-stats
                                       :stats-stream stats-stream
                                       :trace-json-stream trace-json-stream
                                       :print-opt-remarks print-opt-remarks
                                      :opt-remarks-stream opt-remarks-stream
                                      :opt-remarks-mode opt-remarks-mode)))
         (program (compilation-result-program result)))
    (run-compiled program)))

(defun compile-string-with-stdlib (source &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE with standard library prepended."
  (let ((stdlib-forms (get-stdlib-forms))
        (user-forms (parse-all-forms source)))
    (compile-toplevel-forms (append stdlib-forms user-forms)
                            :target target
                            :type-check type-check
                            :safety safety
                            :pass-pipeline pass-pipeline
                            :print-pass-timings print-pass-timings
                            :timing-stream timing-stream
                            :print-pass-stats print-pass-stats
                            :stats-stream stats-stream
                            :trace-json-stream trace-json-stream
                            :print-opt-remarks print-opt-remarks
                            :opt-remarks-stream opt-remarks-stream
                            :opt-remarks-mode opt-remarks-mode)))

(defun our-eval (form)
  "Evaluate FORM by compiling it and running it in the VM.
This is the self-hosting eval — used for compile-time macro expansion
instead of the host CL eval."
  (let* ((result (compile-expression form :target :vm))
         (program (compilation-result-program result)))
    (run-compiled program)))

;;; ─── Self-Hosting Bootstrap ──────────────────────────────────────────────
;;;
;;; Now that compile-expression and run-compiled are available, switch macro
;;; expansion from the host CL eval to our-eval.  From this point on, every
;;; defmacro/macrolet body is compiled and executed by cl-cc's own pipeline —
;;; the fundamental requirement for self-hosting.

(setf *macro-eval-fn* #'our-eval)

;;; REPL persistent state and run-string-repl/our-load are in pipeline-repl.lisp.

(defun run-string-typed (source &key (mode :warn) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile and run SOURCE with type checking enabled.
   MODE is :WARN (default, log warnings) or :STRICT (signal errors)."
  (let* ((result (compile-string source :target :vm :type-check mode
                                  :pass-pipeline pass-pipeline
                                  :print-pass-timings print-pass-timings
                                   :timing-stream timing-stream
                                   :print-pass-stats print-pass-stats
                                   :stats-stream stats-stream
                                   :trace-json-stream trace-json-stream
                                   :print-opt-remarks print-opt-remarks
                                  :opt-remarks-stream opt-remarks-stream
                                  :opt-remarks-mode opt-remarks-mode))
         (program (compilation-result-program result)))
    (values (run-compiled program) (compilation-result-type result))))

