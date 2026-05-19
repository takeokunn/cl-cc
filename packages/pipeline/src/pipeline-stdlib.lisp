;;;; packages/pipeline/src/pipeline-stdlib.lisp — Standard Library Cache Infrastructure
;;;
;;; Contains the stdlib expanded-form cache: snapshot/restore helpers,
;;; cache build, and get-stdlib-forms entry point.
;;;
;;; Why a separate file: the cache subsystem is a self-contained unit
;;; with its own rollback logic; keeping it separate lets pipeline.lisp
;;; focus on compile-expression, compile-string, and run-string.
;;;
;;; This file loads before pipeline.lisp and depends on
;;; stdlib-source{,-ext}.lisp (*standard-library-source*),
;;; expand/macro.lisp (*macro-eval-fn*, compiler-macroexpand-all),
;;; and parse/cl/* (parse-all-forms).
;;; Load order: before pipeline.lisp.

(in-package :cl-cc/pipeline)

;;; ─── Stdlib Expanded-Form Cache ──────────────────────────────────────────

(defparameter *stdlib-expanded-cache* nil
  "Cached list of stdlib forms after parse + macro-expand, but BEFORE
`lower-sexp-to-ast'. Entries are s-expressions copied before each caller hands
them to the normal lowering pipeline.

Rationale for caching at the sexp level rather than post-lower AST:
AST nodes carry identity-sensitive state (gensym registers, slot writers
via defstruct, label counters introduced during lowering) that must NOT
be shared across independent compiler contexts.  A post-lower
cache would hand the same AST node to multiple compilations, yielding colliding
gensyms and shared lowering state.
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

(defparameter *stdlib-cache-directory*
  (merge-pathnames #P".cache/cl-cc/" (user-homedir-pathname))
  "Directory for the readable stdlib expanded-form cache.
The cache stores parse + macro-expanded stdlib sexps so repeated invocations do
not re-read and re-expand the bundled stdlib source unless the stdlib source
files are newer than the cache file.")

(defparameter *stdlib-cache-file-name* "stdlib-expanded-cache.sexp"
  "File name, relative to *STDLIB-CACHE-DIRECTORY*, for the stdlib form cache.")

(defparameter *stdlib-symbol-index* nil
  "Hash table mapping symbols defined by the stdlib to the stdlib source chunk.
Used by lazy auto-require callers to know whether an unresolved reference may be
satisfied by loading the stdlib.")

(defparameter *stdlib-vm-snapshot* nil
  "vm-io-state after compiling and executing *standard-library-source*.
Cloned per run-string call that uses :stdlib t so each test gets an
isolated VM with stdlib functions pre-loaded.")

(defparameter *stdlib-accessor-slot-map* nil
  "Snapshot of *accessor-slot-map* after stdlib compilation.
Copied into each run-string :stdlib t call so defstruct accessors
defined by the stdlib are available during user-form compilation.")

(defparameter *stdlib-defstruct-read-only-accessor-map* nil
  "Snapshot of *defstruct-read-only-accessor-map* after stdlib compilation.
Copied into each run-string :stdlib t call so read-only defstruct accessors
defined by the stdlib are rejected during user-form macro expansion.")

(defparameter *stdlib-defstruct-slot-registry* nil
  "Snapshot of *defstruct-slot-registry* after stdlib compilation.
Copied into each run-string :stdlib t call so struct slot inheritance
defined by the stdlib is available during user-form compilation.")

(defparameter *stdlib-defstruct-type-registry* nil
  "Snapshot of *defstruct-type-registry* after stdlib compilation.
Copied into each run-string :stdlib t call so copy-structure and typed
defstruct metadata remain available during user-form compilation.")

(defparameter *stdlib-setf-compound-place-handlers* nil
  "Snapshot of *setf-compound-place-handlers* after stdlib compilation.
Copied into each run-string :stdlib t call to preserve built-in and stdlib
SETF places while isolating per-source typed defstruct accessors.")

(defun %stdlib-cache-path ()
  "Return the cache file pathname for the serialized stdlib forms."
  (merge-pathnames *stdlib-cache-file-name* *stdlib-cache-directory*))

(defun %stdlib-source-file-paths ()
  "Return known source files that contribute to *STANDARD-LIBRARY-SOURCE*."
  (let ((base (ignore-errors
                (asdf:system-relative-pathname :cl-cc-stdlib #P"src/"))))
    (remove-if-not #'probe-file
                   (when base
                     (mapcar (lambda (name) (merge-pathnames name base))
                             '("stdlib-source-core.lisp"
                               "stdlib-source.lisp"
                               "stdlib-source-ext.lisp"
                               "stdlib-source-clos.lisp"))))))

(defun %stdlib-source-newest-write-date ()
  "Return the newest write date among stdlib source files, or 0 if unknown."
  (loop for path in (%stdlib-source-file-paths)
        for date = (ignore-errors (file-write-date path))
        when date maximize date into newest
        finally (return (or newest 0))))

(defun %stdlib-cache-fresh-p (cache-path)
  "Return T when CACHE-PATH exists and is newer than all stdlib source files."
  (let ((cache-date (and (probe-file cache-path)
                         (ignore-errors (file-write-date cache-path)))))
    (and cache-date
         (>= cache-date (%stdlib-source-newest-write-date)))))

(defun %stdlib-cache-payload-valid-p (payload)
  "Return T when PAYLOAD is a readable stdlib cache payload."
  (and (consp payload)
       (eq (getf payload :format) :cl-cc-stdlib-expanded-cache-v1)
       (listp (getf payload :forms))))

(defun %read-stdlib-expanded-cache-from-disk ()
  "Read cached stdlib expanded forms from disk if the cache is fresh.
Returns NIL on any cache miss or malformed cache."
  (let ((path (%stdlib-cache-path)))
    (when (%stdlib-cache-fresh-p path)
      (ignore-errors
        (with-open-file (in path :direction :input)
          (let* ((serialized (read-line in nil nil))
                 (payload (and serialized (read-from-string serialized))))
            (when (%stdlib-cache-payload-valid-p payload)
              (copy-tree (getf payload :forms)))))))))

(defun %write-stdlib-expanded-cache-to-disk (forms)
  "Serialize FORMS to the stdlib disk cache using WRITE-TO-STRING."
  (let ((path (%stdlib-cache-path)))
    (handler-case
        (progn
          (ensure-directories-exist path)
          (with-open-file (out path
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (write-line
             (write-to-string
              (list :format :cl-cc-stdlib-expanded-cache-v1
                    :source-newest-write-date (%stdlib-source-newest-write-date)
                    :forms forms)
              :readably t
              :circle t)
             out)))
      (error (c)
        (warn "Cannot persist stdlib cache (~A): ~A" path c)))))

(defun %stdlib-definition-symbol (form)
  "Return the primary symbol defined by top-level stdlib FORM, if any."
  (when (and (consp form) (symbolp (car form)))
    (case (car form)
      ((defun defmacro defclass defgeneric defstruct defvar defparameter defconstant)
       (and (symbolp (second form)) (second form)))
      (otherwise nil))))

(defun %rebuild-stdlib-symbol-index (forms)
  "Rebuild *STDLIB-SYMBOL-INDEX* from parsed/expanded stdlib FORMS."
  (let ((index (make-hash-table :test #'eq)))
    (dolist (form forms)
      (let ((symbol (%stdlib-definition-symbol form)))
        (when symbol
          (setf (gethash symbol index) :standard-library))))
    (setf *stdlib-symbol-index* index)))

(defun stdlib-symbol-defined-p (symbol)
  "Return T when SYMBOL is known to be provided by the stdlib."
  (and *stdlib-symbol-index*
       (gethash symbol *stdlib-symbol-index*)
       t))

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
If the fresh-build step throws, the live table is untouched (the original
torn state remains observable, but no worse than pre-call).  Building first
avoids leaving the live table empty if `maphash' re-population throws
(e.g., OOM) during the commit step.

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

INVARIANT: returned list contains ONLY sexps, never AST nodes.  Shared AST
nodes carry identity-sensitive gensym registers and can collide across
independent compilations, so the assertion below preserves the sexp-only
cache contract."
  (let ((snapshot (%snapshot-macro-env-table))
        (success-p nil)
        (*package* (or (find-package :cl-cc) *package*)))
    (unwind-protect
         (let ((result (mapcar #'compiler-macroexpand-all
                                (parse-all-forms *standard-library-source*))))
            ;; Tripwire: stdlib source is plain text; every entry MUST be a sexp.
            (dolist (form result)
              (when (typep form 'ast-node)
                (error "stdlib cache builder produced an AST node; only sexps are allowed in the shared stdlib cache")))
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
      (setf *stdlib-expanded-cache*         (or (%read-stdlib-expanded-cache-from-disk)
                                                (let ((forms (%build-stdlib-expanded-cache)))
                                                  (%write-stdlib-expanded-cache-to-disk forms)
                                                  forms))
            *stdlib-expanded-cache-source*  src-at-entry
            *stdlib-expanded-cache-eval-fn* eval-fn-at-entry)
      (%rebuild-stdlib-symbol-index *stdlib-expanded-cache*)))
  (copy-tree *stdlib-expanded-cache*))
