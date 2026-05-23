;;;; packages/codegen/src/x86-64-eh.lisp — zero-cost EH landing-pad infrastructure
;;;;
;;;; FR-562: Landing-pad generation for handler-case/restart-case native sites.
;;;; Default compilation remains SJLJ/VM-compatible: all machinery is gated by
;;;; *ZERO-COST-EH-ENABLED* and is NIL by default.

(in-package :cl-cc/codegen)

(defparameter *zero-cost-eh-enabled* nil
  "When true, emit experimental x86-64 table-based EH landing-pad metadata/code.

The default NIL preserves the existing SJLJ-style condition/restart path and
keeps the happy path at zero additional instructions.")

(defparameter *eh-model* :sjlj
  "Exception handling model selected by the native pipeline.

:SJLJ keeps the legacy VM-compatible handler stack.  :TABLE enables the
Itanium/DWARF zero-cost path backed by .eh_frame, LSDA call-site tables, and a
personality routine.  *ZERO-COST-EH-ENABLED* is retained for compatibility and
is treated as an additional opt-in switch.")

(defun normalize-x86-64-eh-model (model)
  "Normalize MODEL from CLI/pipeline input to :SJLJ or :TABLE."
  (cond
    ((or (null model) (eq model :sjlj)) :sjlj)
    ((eq model :table) :table)
    ((stringp model)
     (let ((down (string-downcase model)))
       (cond ((member down '("sjlj" "setjmp" "setjmp-longjmp") :test #'string=) :sjlj)
             ((member down '("table" "zero-cost" "dwarf" "itanium") :test #'string=) :table)
             (t (error "Unknown x86-64 EH model: ~A" model)))))
    (t (error "Unknown x86-64 EH model: ~S" model))))

(defun x86-64-table-eh-enabled-p ()
  "Return true when the backend should emit table-based exception metadata."
  (or *zero-cost-eh-enabled* (eq (normalize-x86-64-eh-model *eh-model*) :table)))

;;; Itanium C++ ABI / System V _Unwind constants.  Common Lisp exceptions use
;;; the same two-phase unwinder protocol as C++:
;;;
;;;   1. _Unwind_RaiseException starts the SEARCH phase.  Each frame's
;;;      personality inspects its LSDA and either returns CONTINUE_UNWIND or
;;;      HANDLER_FOUND.
;;;   2. The unwinder restarts in CLEANUP phase.  Cleanup frames run cleanup
;;;      landing pads; the handler frame is marked with HANDLER_FRAME.
;;;   3. The handler frame's personality installs the landing-pad register/IP
;;;      state and returns INSTALL_CONTEXT, transferring control to the pad.
;;;
;;; cl-cc's landing pads retrieve the CL condition object from the foreign
;;; exception wrapper (_Unwind_GetException / exception_object private payload),
;;; parse the function LSDA, check the type table against CL condition types,
;;; and jump to the handler label selected by the call-site table.
(defconstant +_ua-search-phase+ #x01)
(defconstant +_ua-cleanup-phase+ #x02)
(defconstant +_ua-handler-frame+ #x04)
(defconstant +_ua-force-unwind+ #x08)

(defconstant +_urc-no-reason+ 0)
(defconstant +_urc-foreign-exception-caught+ 1)
(defconstant +_urc-fatal-phase2-error+ 2)
(defconstant +_urc-fatal-phase1-error+ 3)
(defconstant +_urc-normal-stop+ 4)
(defconstant +_urc-end-of-stack+ 5)
(defconstant +_urc-handler-found+ 6)
(defconstant +_urc-install-context+ 7)
(defconstant +_urc-continue-unwind+ 8)

(defconstant +clcc-exception-class+ #x434c434345584330
  "Eight-byte exception class tag: \"CLCCEXC0\".")

(defconstant +x86-64-landing-pad-size+ 16
  "Reserved byte size for each x86-64 landing pad.")

(defconstant +x86-64-landing-pad-stub-size+ +x86-64-landing-pad-size+
  "Alias for +x86-64-landing-pad-size+ used by EH stub emission tests.")

(defstruct x86-64-landing-pad
  "Metadata for one protected native handler/restart landing pad."
  (start-address 0 :type integer)
  (end-address 0 :type integer)
  (handler-type t)
  (handler-address 0 :type integer)
  (handler-label nil)
  (result-register nil)
  (kind :handler :type symbol))

(defstruct x86-64-personality-descriptor
  "Personality-function descriptor for libunwind LSDA integration."
  (name "clcc_personality_v0" :type string)
  (foreign-equivalent "__gxx_personality_v0" :type string)
  (raise-exception "_Unwind_RaiseException" :type string)
  (get-exception "_Unwind_GetException" :type string)
  (get-lsda "_Unwind_GetLanguageSpecificData" :type string)
  (get-region-start "_Unwind_GetRegionStart" :type string)
  (get-ip "_Unwind_GetIP" :type string)
  (set-gr "_Unwind_SetGR" :type string)
  (set-ip "_Unwind_SetIP" :type string))

(defparameter *x86-64-personality-descriptor*
  (make-x86-64-personality-descriptor)
  "Personality routine descriptor used by x86-64 table-based EH.")

(defstruct x86-64-eh-dispatch
  "Pure model of the personality decision for one frame."
  (return-code +_urc-continue-unwind+ :type integer)
  (landing-pad nil)
  (matched-type nil)
  (exception nil)
  (ip 0 :type integer))

(defun x86-64-eh-condition-matches-p (exception handler-type)
  "Return true when EXCEPTION satisfies HANDLER-TYPE.

HANDLER-TYPE is the CL condition type carried in the LSDA type table.  T and
CONDITION are catch-all entries; symbols are checked with TYPEP so user-defined
condition classes participate in normal CLOS condition dispatch."
  (cond
    ((or (eq handler-type t) (eq handler-type 'condition)) t)
    ((or (symbolp handler-type) (typep handler-type 'class))
     (handler-case (typep exception handler-type)
       (error () nil)))
    (t nil)))

(defun x86-64-find-landing-pad (landing-pads absolute-ip exception &key (region-start 0))
  "Parse the call-site table model and select a landing pad for EXCEPTION.

ABSOLUTE-IP is the value returned by _Unwind_GetIP.  REGION-START is the value
returned by _Unwind_GetRegionStart.  The LSDA stores offsets relative to that
region, so this helper mirrors the runtime personality lookup in Lisp for tests
and for metadata generation sanity checks."
  (let ((ip (- absolute-ip region-start)))
    (find-if (lambda (lp)
               (and (<= (x86-64-landing-pad-start-address lp) ip)
                    (< ip (x86-64-landing-pad-end-address lp))
                    (x86-64-eh-condition-matches-p
                     exception (x86-64-landing-pad-handler-type lp))))
             landing-pads)))

(defun x86-64-personality-dispatch (actions landing-pads exception absolute-ip
                                     &key (region-start 0))
  "Implement the Itanium personality state machine for cl-cc metadata.

This is the executable Lisp model of clcc_personality_v0.  The separately
emitted native stub follows the same decisions: SEARCH reports a matching
handler, CLEANUP continues through cleanup frames, and HANDLER_FRAME installs
the selected landing pad context."
  (let ((lp (x86-64-find-landing-pad landing-pads absolute-ip exception
                                     :region-start region-start)))
    (cond
      ((and (not (zerop (logand actions +_ua-search-phase+))) lp)
       (make-x86-64-eh-dispatch :return-code +_urc-handler-found+
                                :landing-pad lp :matched-type (x86-64-landing-pad-handler-type lp)
                                :exception exception :ip absolute-ip))
      ((and (not (zerop (logand actions +_ua-cleanup-phase+)))
            (not (zerop (logand actions +_ua-handler-frame+)))
            lp)
       (make-x86-64-eh-dispatch :return-code +_urc-install-context+
                                :landing-pad lp :matched-type (x86-64-landing-pad-handler-type lp)
                                :exception exception :ip absolute-ip))
      (t
       (make-x86-64-eh-dispatch :return-code +_urc-continue-unwind+
                                :exception exception :ip absolute-ip)))))

(defun x86-64-lsda-bytes (landing-pads)
  "Build a DWARF LSDA byte vector for LANDING-PADS."
  (cl-cc/binary:build-dwarf-eh-lsda
   (mapcar (lambda (lp)
             (cl-cc/binary:make-dwarf-eh-call-site
              :start (x86-64-landing-pad-start-address lp)
              :length (max 0 (- (x86-64-landing-pad-end-address lp)
                                (x86-64-landing-pad-start-address lp)))
              :landing-pad (x86-64-landing-pad-handler-address lp)
              :action 1
              :type (x86-64-landing-pad-handler-type lp)
              :cleanup-p (eq (x86-64-landing-pad-kind lp) :restart)))
           landing-pads)))

(defun emit-x86-64-personality-function (stream)
  "Emit a compact native clcc_personality_v0 dispatcher stub.

The production linker resolves the named _Unwind_* helpers in
*X86-64-PERSONALITY-STUB*.  In this pure byte emitter we cannot attach symbol
relocations, so the bytes encode the ABI-level control-flow skeleton: inspect
the ACTIONS argument (RSI), return HANDLER_FOUND for SEARCH, INSTALL_CONTEXT
for CLEANUP|HANDLER_FRAME, and CONTINUE_UNWIND otherwise.  The Lisp model above
performs LSDA/type dispatch; binary metadata carries the same LSDA for real
libunwind consumers."
  ;; test rsi, _UA_SEARCH_PHASE
  (emit-byte #x48 stream) (emit-byte #xF7 stream) (emit-byte #xC6 stream)
  (emit-dword +_ua-search-phase+ stream)
  ;; jz +6
  (emit-byte #x74 stream) (emit-byte #x06 stream)
  ;; mov eax, _URC_HANDLER_FOUND; ret
  (emit-byte #xB8 stream) (emit-dword +_urc-handler-found+ stream) (emit-byte #xC3 stream)
  ;; test rsi, _UA_HANDLER_FRAME
  (emit-byte #x48 stream) (emit-byte #xF7 stream) (emit-byte #xC6 stream)
  (emit-dword +_ua-handler-frame+ stream)
  ;; jz +6
  (emit-byte #x74 stream) (emit-byte #x06 stream)
  ;; mov eax, _URC_INSTALL_CONTEXT; ret
  (emit-byte #xB8 stream) (emit-dword +_urc-install-context+ stream) (emit-byte #xC3 stream)
  ;; mov eax, _URC_CONTINUE_UNWIND; ret
  (emit-byte #xB8 stream) (emit-dword +_urc-continue-unwind+ stream) (emit-byte #xC3 stream))

(defun x86-64-eh-site-instruction-p (inst)
  "Return T when INST establishes a handler-case/restart-case landing site."
  (or (typep inst 'cl-cc/vm::vm-push-handler)
      (typep inst 'cl-cc/vm::vm-establish-handler)
      (typep inst 'cl-cc/vm::vm-bind-restart)))

(defun x86-64-eh-site-kind (inst)
  "Classify a handler/restart site instruction."
  (cond
    ((typep inst 'cl-cc/vm::vm-bind-restart) :restart)
    (t :handler)))

(defun x86-64-eh-site-handler-type (inst)
  "Return the condition/restart type associated with INST, if known."
  (cond
    ((typep inst 'cl-cc/vm::vm-push-handler)
     (cl-cc/vm::vm-push-handler-type inst))
    ((typep inst 'cl-cc/vm::vm-establish-handler)
     'condition)
    ((typep inst 'cl-cc/vm::vm-bind-restart)
     (cl-cc/vm::vm-restart-name-inst inst))
    (t t)))

(defun x86-64-eh-site-handler-label (inst)
  "Return the VM label associated with INST's handler/restart path, if known."
  (cond
    ((or (typep inst 'cl-cc/vm::vm-push-handler)
         (typep inst 'cl-cc/vm::vm-establish-handler))
     (cl-cc/vm::vm-handler-label inst))
    ((typep inst 'cl-cc/vm::vm-bind-restart)
     (cl-cc/vm::vm-restart-label inst))
    (t nil)))

(defun x86-64-eh-site-result-register (inst)
  "Return the result register populated when control enters the handler."
  (cond
    ((or (typep inst 'cl-cc/vm::vm-push-handler)
         (typep inst 'cl-cc/vm::vm-establish-handler))
     (cl-cc/vm::vm-handler-result-reg inst))
    (t nil)))

(defun x86-64-build-landing-pad-table (instructions label-offsets prologue-size)
  "Build landing-pad table rows for handler/restart sites.

Rows record protected range start/end, handler type, and handler address.  The
range begins at the establishing instruction and conservatively extends to the
handler target when available; this keeps the table useful without changing SJLJ
semantics."
  (let ((rows nil)
        (pos prologue-size))
    (dolist (inst instructions (nreverse rows))
      (when (x86-64-eh-site-instruction-p inst)
        (let* ((label (x86-64-eh-site-handler-label inst))
               (handler-address (and label (gethash label label-offsets))))
          (push (make-x86-64-landing-pad
                 :start-address pos
                 :end-address (or handler-address (+ pos (instruction-size inst)))
                 :handler-type (x86-64-eh-site-handler-type inst)
                 :handler-address (or handler-address 0)
                 :handler-label label
                 :result-register (x86-64-eh-site-result-register inst)
                 :kind (x86-64-eh-site-kind inst))
                rows)))
      (incf pos (instruction-size inst)))))

(defun x86-64-landing-pad-table->dwarf-fdes (landing-pads)
  "Convert LANDING-PADS to DWARF .eh_frame FDE models for binary integration."
  (mapcar (lambda (lp)
            (cl-cc/binary:make-dwarf-eh-fde
             :initial-location (x86-64-landing-pad-start-address lp)
              :address-range (max 0 (- (x86-64-landing-pad-end-address lp)
                                        (x86-64-landing-pad-start-address lp)))
              :personality (x86-64-personality-descriptor-name *x86-64-personality-descriptor*)
              :lsda (x86-64-lsda-bytes landing-pads)
              :instructions '((:def-cfa-offset 8))))
          landing-pads))

(defun emit-x86-64-landing-pad-stub (landing-pad stream)
  "Emit one x86-64 landing-pad transfer stub.

The table-based EH path is still opt-in, but the emitted landing pad is real
control-transfer code: it tail-jumps to the resolved handler label address.  A
missing handler address falls back to RET so accidental entry does not execute a
UD2-only placeholder.  The fixed 16-byte footprint keeps existing offset/table
assumptions stable."
  (let ((handler-address (x86-64-landing-pad-handler-address landing-pad)))
    (if (and (integerp handler-address) (plusp handler-address))
        (progn
          ;; movabs r11, imm64  => 49 BB imm64
          (emit-byte #x49 stream)
          (emit-byte #xBB stream)
          (dotimes (i 8)
            (emit-byte (ldb (byte 8 (* i 8)) handler-address) stream))
          ;; jmp r11 => 41 FF E3
          (emit-byte #x41 stream)
          (emit-byte #xFF stream)
          (emit-byte #xE3 stream)
          (loop repeat 3 do (emit-byte #x90 stream)))
        (progn
          ;; ret + NOP padding: safe fallback when metadata lacks a target.
          (emit-byte #xC3 stream)
          (loop repeat (1- +x86-64-landing-pad-size+)
                do (emit-byte #x90 stream))))))

(defun emit-x86-64-landing-pads (landing-pads stream)
  "Emit all LANDING-PADS when zero-cost EH is enabled."
  (when (x86-64-table-eh-enabled-p)
    (dolist (landing-pad landing-pads)
      (emit-x86-64-landing-pad-stub landing-pad stream))))
