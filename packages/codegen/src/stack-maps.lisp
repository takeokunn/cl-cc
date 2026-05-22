;;;; stack-maps.lisp — FR-541 safepoint stack maps for precise GC

(in-package :cl-cc/codegen)

(defparameter *precise-gc-stack-maps-enabled* nil
  "When true, emit FR-541 stack-map records after safepoint instructions.

The default is NIL so existing code generation and the 7746/0 baseline remain
unchanged unless precise GC metadata is explicitly requested.")

(defstruct (cg-stack-map-entry (:conc-name cg-sme-))
  "One precise GC root location at a safepoint."
  (kind :register :type keyword)
  (location nil)
  (index 0 :type fixnum))

(defstruct (cg-stack-map (:conc-name cg-sm-))
  "Precise root map for one safepoint in a generated code section."
  (safepoint-id 0 :type fixnum)
  (pc-offset 0 :type fixnum)
  (register-roots nil :type list)
  (stack-slot-roots nil :type list)
  (register-bitmask 0 :type integer)
  (stack-bitmask 0 :type integer)
  (frame-size 0 :type fixnum))

(defun %cg-root-keyword-p (value)
  (and (keywordp value)
       (or (search "ROOT" (symbol-name value))
           (search "POINTER" (symbol-name value))
           (search "OBJECT" (symbol-name value))
           (search "GC" (symbol-name value)))))

(defun %cg-root-type-p (value)
  "Conservatively recognize values known to hold heap pointers."
  (or (eq value :pointer)
      (eq value :object)
      (%cg-root-keyword-p value)))

(defun %cg-plist-value (plist key)
  (when (listp plist)
    (loop for rest = plist then (cddr rest)
          while (consp rest)
          when (eq (car rest) key)
            do (return (cadr rest)))))

(defun %cg-mir-register-root-p (operand)
  (and (typep operand 'mir-value)
       (%cg-root-type-p (mirv-type operand))))

(defun %cg-mir-stack-root-p (entry)
  (and (consp entry)
       (or (eq (getf entry :kind) :stack)
           (getf entry :slot))
       (%cg-root-type-p (or (getf entry :type) (getf entry :kind)))))

(defun %cg-bitmask-for-indexes (indexes)
  (reduce (lambda (mask index)
            (if (and (integerp index) (<= 0 index 62))
                (logior mask (ash 1 index))
                mask))
          indexes
          :initial-value 0))

(defun %cg-register-index (reg fallback)
  (cond
    ((integerp reg) reg)
    ((and (typep reg 'mir-value) (integerp (mirv-id reg))) (mirv-id reg))
    (t fallback)))

(defun cg-generate-safepoint-stack-map (safepoint &key (safepoint-id 0) (pc-offset 0) frame-size live-registers live-stack-slots)
  "Generate a FR-541 precise GC stack map for SAFEPOINT.

LIVE-REGISTERS and LIVE-STACK-SLOTS may be supplied by a register allocator.  If
omitted, MIR safepoint metadata is inspected for :GC-ROOTS, :LIVE-REGISTERS,
:STACK-SLOTS, and :FRAME-SIZE.  Roots are encoded both as arrays/lists and as
compact bitmasks for native code sections."
  (let* ((meta (and (typep safepoint 'mir-inst) (miri-meta safepoint)))
         (gc-roots (or (%cg-plist-value meta :gc-roots)
                       (%cg-plist-value meta :roots)))
         (regs (or live-registers
                   (%cg-plist-value meta :live-registers)
                   (remove-if-not #'%cg-mir-register-root-p
                                  (and (typep safepoint 'mir-inst) (miri-srcs safepoint)))))
         (slots (or live-stack-slots
                    (%cg-plist-value meta :stack-slots)
                    (remove-if-not #'%cg-mir-stack-root-p gc-roots)))
         (reg-entries (loop for reg in regs
                            for i from 0
                            for index = (%cg-register-index reg i)
                            collect (make-cg-stack-map-entry :kind :register
                                                             :location reg
                                                             :index index)))
         (slot-entries (loop for slot in slots
                             for i from 0
                             for index = (or (and (consp slot) (getf slot :slot))
                                             (and (consp slot) (getf slot :offset))
                                             (and (integerp slot) slot)
                                             i)
                             collect (make-cg-stack-map-entry :kind :stack-slot
                                                              :location slot
                                                              :index index))))
    (make-cg-stack-map
     :safepoint-id safepoint-id
     :pc-offset pc-offset
     :register-roots reg-entries
     :stack-slot-roots slot-entries
     :register-bitmask (%cg-bitmask-for-indexes (mapcar #'cg-sme-index reg-entries))
     :stack-bitmask (%cg-bitmask-for-indexes (mapcar #'cg-sme-index slot-entries))
     :frame-size (or frame-size (%cg-plist-value meta :frame-size) 0))))

(defun cg-stack-map->section-record (stack-map)
  "Return the compact code-section payload for STACK-MAP."
  (list :gc-stack-map
        :safepoint-id (cg-sm-safepoint-id stack-map)
        :pc-offset (cg-sm-pc-offset stack-map)
        :register-bitmask (cg-sm-register-bitmask stack-map)
        :stack-bitmask (cg-sm-stack-bitmask stack-map)
        :frame-size (cg-sm-frame-size stack-map)
        :registers (mapcar #'cg-sme-location (cg-sm-register-roots stack-map))
        :stack-slots (mapcar #'cg-sme-location (cg-sm-stack-slot-roots stack-map))))

(defun cg-safepoint-instruction-p (inst)
  "Return T when INST is a MIR/list safepoint instruction."
  (or (and (typep inst 'mir-inst) (eq (miri-op inst) :safepoint))
      (and (consp inst) (eq (first inst) :safepoint))))

(defun cg-embed-stack-maps-after-safepoints (code-section)
  "Embed stack-map records immediately after safepoints in CODE-SECTION.

When *PRECISE-GC-STACK-MAPS-ENABLED* is NIL this is an identity function."
  (if (not *precise-gc-stack-maps-enabled*)
      code-section
      (let ((out nil)
            (pc 0)
            (id 0))
        (dolist (inst code-section (nreverse out))
          (push inst out)
          (when (cg-safepoint-instruction-p inst)
            (let ((map (cg-generate-safepoint-stack-map inst
                                                        :safepoint-id id
                                                        :pc-offset pc)))
              (push (cg-stack-map->section-record map) out)
              (incf id)))
          (incf pc)))))
