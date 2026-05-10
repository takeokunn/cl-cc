;;;; inference-forms-advanced-validators.lisp — Per-feature advanced call validators
;;;; Extracted from inference-forms-advanced.lisp.
;;;; Load order: after inference-forms-advanced.
(in-package :cl-cc/type)

(defun %validate-advanced-spawn-call (ast arg-types _env)
  "Enforce Send for values crossing a spawn boundary."
  (declare (ignore ast _env))
  (loop for ty in arg-types
        for designator = (%advanced-call-type-designator ty)
        unless (and designator (sendable-type-p designator))
          do (%advanced-call-error 'spawn
                                   "argument type ~A is not Send"
                                   (type-to-string ty))))

(defun %validate-advanced-shared-ref-call (ast arg-types _env)
  "Enforce Sync for values shared by reference."
  (declare (ignore ast _env))
  (loop for ty in arg-types
        for designator = (%advanced-call-type-designator ty)
        unless (and designator (shareable-type-p designator))
          do (%advanced-call-error 'shared-ref
                                   "argument type ~A is not Sync"
                                   (type-to-string ty))))

(defun %validate-advanced-ffi-call (ast arg-types _env)
  "Validate typed FFI descriptors at inference time."
  (declare (ignore _env))
  (let* ((descriptor-form (%advanced-call-quoted-arg ast 0 'foreign-call))
         (descriptor (ffi-descriptor-from-form descriptor-form))
         (argument-descriptors (if (ffi-function-descriptor-p descriptor)
                                    (ffi-function-descriptor-argument-types descriptor)
                                    (list descriptor)))
         (runtime-arg-types (rest arg-types)))
    (unless (ffi-descriptor-form-valid-p descriptor)
      (%advanced-call-error 'foreign-call "malformed FFI descriptor ~S" descriptor-form))
    (unless (= (length argument-descriptors) (length runtime-arg-types))
      (%advanced-call-error 'foreign-call
                            "descriptor expects ~D argument(s), got ~D"
                            (length argument-descriptors)
                            (length runtime-arg-types)))
    (loop for arg-type in runtime-arg-types
          for descriptor-type in argument-descriptors
          for designator = (%advanced-call-type-designator arg-type)
          unless (and designator
                      (ffi-lisp-type-compatible-p designator descriptor-type))
            do (%advanced-call-error 'foreign-call
                                     "argument type ~A is incompatible with FFI descriptor ~S"
                                     (type-to-string arg-type)
                                     descriptor-type))
    (if (ffi-function-descriptor-p descriptor)
        (ffi-descriptor-lisp-type (ffi-function-descriptor-return-type descriptor))
        type-any)))

(defun %validate-advanced-interface-call (ast _arg-types _env)
  "Validate interface-file metadata before accepting an interface load."
  (declare (ignore _arg-types _env))
  (let ((module (%advanced-call-quoted-arg ast 0 'load-type-interface))
        (exports (%advanced-call-quoted-arg ast 1 'load-type-interface))
        (fingerprint (%advanced-call-quoted-arg ast 2 'load-type-interface)))
    (register-type-interface module exports fingerprint)
    (make-type-advanced :feature-id "FR-2405"
                        :name 'interface-file
                        :args (list module)
                        :properties (list (cons :exports exports)
                                          (cons :fingerprint fingerprint)))
    type-symbol))

(defun %validate-advanced-smt-call (ast _arg-types _env)
  "Validate SMT solver/theory metadata before accepting an SMT assertion."
  (declare (ignore _arg-types _env))
  (let ((constraint (%advanced-call-quoted-arg ast 0 'smt-assert))
        (solver (%advanced-call-quoted-arg ast 1 'smt-assert))
        (theory (%advanced-call-quoted-arg ast 2 'smt-assert)))
    (solve-smt-constraint constraint solver theory)
    (make-type-advanced :feature-id "FR-2406"
                        :name 'smt
                        :args (list constraint)
                        :properties (list (cons :solver solver)
                                          (cons :theory theory)
                                          (cons :counterexample :none)))
    type-bool))

(defun %validate-advanced-plugin-call (ast _arg-types _env)
  "Validate type-checker plugin hook metadata at inference time."
  (let ((plugin (%advanced-call-quoted-arg ast 0 'run-type-plugin))
        (phase (%advanced-call-quoted-arg ast 1 'run-type-plugin)))
    (let ((result (run-type-checker-plugin plugin phase ast _arg-types _env)))
      (make-type-advanced :feature-id "FR-3002"
                          :name 'type-checker-plugin
                          :args (list plugin)
                          :properties (list (cons :hook plugin)
                                            (cons :phase phase)
                                            (cons :result result)))
      (or (getf result :type) type-any))))

(defun %validate-advanced-synthesis-call (ast _arg-types _env)
  "Validate type-directed synthesis metadata at inference time."
  (declare (ignore _arg-types _env))
  (let ((signature (%advanced-call-quoted-arg ast 0 'synthesize-program))
        (strategy (%advanced-call-quoted-arg ast 1 'synthesize-program))
        (fuel (%advanced-call-quoted-arg ast 2 'synthesize-program)))
    (run-type-synthesis signature strategy fuel)
    (make-type-advanced :feature-id "FR-3003"
                        :name 'program-synthesis
                        :args (list signature)
                        :properties (list (cons :search strategy)
                                          (cons :fuel fuel)))
    (%advanced-call-parse-type-form signature 'synthesize-program)))

(defun %validate-advanced-mapped-type-call (ast _arg-types _env)
  "Validate mapped type evaluation metadata at inference time."
  (declare (ignore _arg-types _env))
  (let* ((base-form (%advanced-call-quoted-arg ast 0 'apply-mapped-type))
         (transform (%advanced-call-quoted-arg ast 1 'apply-mapped-type))
         (base (%advanced-call-parse-type-form base-form 'apply-mapped-type)))
    (make-type-advanced :feature-id "FR-3301"
                        :name 'mapped-type
                        :args (list base-form)
                        :properties (list (cons :transform transform)))
    (%advanced-call-apply-mapped-transform base transform 'apply-mapped-type)))

(defun %validate-advanced-conditional-type-call (ast _arg-types _env)
  "Validate conditional type evaluation metadata at inference time."
  (declare (ignore _arg-types _env))
  (let* ((base-form (%advanced-call-quoted-arg ast 0 'apply-conditional-type))
         (extends-form (%advanced-call-quoted-arg ast 1 'apply-conditional-type))
         (infer-var (%advanced-call-quoted-arg ast 2 'apply-conditional-type))
         (then-branch (%advanced-call-quoted-arg ast 3 'apply-conditional-type))
         (else-branch (%advanced-call-quoted-arg ast 4 'apply-conditional-type))
         (base (%advanced-call-parse-type-form base-form 'apply-conditional-type))
         (extends (%advanced-call-parse-type-form extends-form 'apply-conditional-type)))
    (make-type-advanced :feature-id "FR-3302"
                        :name 'conditional-type
                        :args (list base-form)
                        :properties (list (cons :extends extends)
                                          (cons :infer infer-var)
                                          (cons :then then-branch)
                                          (cons :else else-branch)))
    (if (%advanced-call-type-extends-p base extends)
        (%advanced-call-inferred-branch-type then-branch infer-var base 'apply-conditional-type)
        (%advanced-call-inferred-branch-type else-branch infer-var base 'apply-conditional-type))))
