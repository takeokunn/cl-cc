(in-package :cl-cc/pipeline)

;;; FR-525 — AutoFDO / sample-based PGO infrastructure.
;;;
;;; This is an ingestion/annotation layer.  It accepts simple AutoFDO text rows,
;;; `perf script`-style rows, and a conservative binary perf.data record scan for
;;; PERF_RECORD_MMAP/PERF_RECORD_SAMPLE.  Instruction pointers are mapped through
;;; FR-553 perf-map entries first, with addr2line as an optional fallback.

(defstruct (autofdo-profile (:constructor make-autofdo-profile))
  (function-hotness nil)
  (branch-probabilities nil)
  (raw-samples nil)
  (perf-map nil)
  (layout-decisions nil))

(defconstant +perf-record-mmap+ 1)
(defconstant +perf-record-sample+ 9)

(defun %autofdo-read-file-bytes (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
      (let* ((size (file-length in))
             (bytes (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence bytes in)
        bytes))))

(defun %autofdo-u16le (bytes pos)
  (when (<= (+ pos 2) (length bytes))
    (logior (aref bytes pos)
            (ash (aref bytes (1+ pos)) 8))))

(defun %autofdo-u32le (bytes pos)
  (when (<= (+ pos 4) (length bytes))
    (logior (aref bytes pos)
            (ash (aref bytes (+ pos 1)) 8)
            (ash (aref bytes (+ pos 2)) 16)
            (ash (aref bytes (+ pos 3)) 24))))

(defun %autofdo-u64le (bytes pos)
  (when (<= (+ pos 8) (length bytes))
    (logior (%autofdo-u32le bytes pos)
            (ash (%autofdo-u32le bytes (+ pos 4)) 32))))

(defun %autofdo-printable-byte-p (byte)
  (or (<= 32 byte 126) (member byte '(9 10 13))))

(defun %autofdo-binary-profile-p (bytes)
  "Return true when BYTES look like perf.data or non-text profile data."
  (and bytes
       (or (and (>= (length bytes) 8)
                (string= (map 'string #'code-char (subseq bytes 0 8)) "PERFILE2"))
           (loop for i below (min (length bytes) 256)
                 thereis (not (%autofdo-printable-byte-p (aref bytes i)))))))

(defun %autofdo-parse-int (token &key (radix 10))
  (ignore-errors (parse-integer token :radix radix :junk-allowed t)))

(defun %autofdo-hex-token-p (token)
  (and (stringp token)
       (> (length token) 0)
       (every (lambda (ch) (or (digit-char-p ch 16) (member ch '(#\x #\X)))) token)))

(defun %autofdo-parse-ip (token)
  (let ((text (string-trim '(#\Space #\Tab #\:) token)))
    (cond
      ((and (> (length text) 2)
            (char= (char text 0) #\0)
            (member (char text 1) '(#\x #\X)))
       (%autofdo-parse-int (subseq text 2) :radix 16))
      ((%autofdo-hex-token-p text)
       (%autofdo-parse-int text :radix 16))
      (t (%autofdo-parse-int text)))))

(defun read-perf-map (&optional (path (perf-map-path)))
  "Read FR-553 perf-map PATH and return entries (START END SYMBOL)."
  (let ((entries nil))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line
              when (perf-map-line-valid-p line)
                do (let* ((parts (uiop:split-string line :separator '(#\Space #\Tab)))
                          (start (%autofdo-parse-int (first parts) :radix 16))
                          (size (%autofdo-parse-int (second parts) :radix 16))
                          (name (third parts)))
                     (when (and start size name)
                       (push (list start (+ start size) name) entries))))))
    (sort entries #'< :key #'first)))

(defun autofdo-map-ip-to-function (ip perf-map)
  "Map sample IP to a source/function name using PERF-MAP entries."
  (let ((entry (find-if (lambda (row)
                         (and (<= (first row) ip) (< ip (second row))))
                       perf-map)))
    (and entry (third entry))))

(defun %autofdo-addr2line-function (ip executable-path)
  "Resolve IP with addr2line when EXECUTABLE-PATH is available."
  (when (and executable-path (probe-file executable-path))
    (handler-case
        (let ((out (make-string-output-stream)))
          (let ((proc (sb-ext:run-program "addr2line"
                                          (list "-f" "-e" (namestring executable-path)
                                                (format nil "0x~X" ip))
                                          :output out :error nil :search t)))
            (when (zerop (sb-ext:process-exit-code proc))
              (let* ((text (get-output-stream-string out))
                     (lines (uiop:split-string text :separator '(#\Newline #\Return)))
                     (fn (first (remove "" lines :test #'string=))))
                (and fn (not (string= fn "??")) fn)))))
      (error () nil))))

(defun %autofdo-map-ip (ip perf-map executable-path)
  (or (autofdo-map-ip-to-function ip perf-map)
      (%autofdo-addr2line-function ip executable-path)))

(defun %autofdo-record-function-sample (table function count)
  (when function
    (incf (gethash function table 0) count)))

(defun %autofdo-c-string (bytes start end)
  (let ((limit (or (position 0 bytes :start start :end end) end)))
    (map 'string #'code-char (subseq bytes start limit))))

(defun %autofdo-parse-perf-mmap-record (bytes pos size)
  "Parse basic PERF_RECORD_MMAP payload into a perf-map-like row."
  (let* ((payload (+ pos 8))
         (end (+ pos size))
         (addr (%autofdo-u64le bytes (+ payload 8)))
         (len (%autofdo-u64le bytes (+ payload 16)))
         (filename-start (+ payload 32)))
    (when (and addr len (<= filename-start end))
      (list addr (+ addr len) (%autofdo-c-string bytes filename-start end)))))

(defun %autofdo-parse-perf-sample-record (bytes pos size)
  "Parse a basic PERF_RECORD_SAMPLE payload, treating the first u64 as IP."
  (let ((ip (%autofdo-u64le bytes (+ pos 8))))
    (when (and ip (plusp ip))
      (list :ip ip :count 1 :record-size size))))

(defun read-perf-data-binary (profile-path &key perf-map-path executable-path)
  "Read binary perf.data enough to consume PERF_RECORD_MMAP and SAMPLE rows.

perf.data sample layouts depend on perf_event_attr.sample_type.  This parser is
deliberately conservative: it scans plausible perf_event_header records and reads
the common leading IP field from SAMPLE payloads.  MMAP records augment FR-553
perf-map rows for symbolization.  Malformed or unsupported files return an empty
profile instead of failing compilation."
  (let* ((bytes (%autofdo-read-file-bytes profile-path))
         (map (append (read-perf-map (or perf-map-path (perf-map-path))) nil))
         (hotness (make-hash-table :test #'equal))
         (samples nil))
    (when bytes
      (let ((pos 0)
            (limit (length bytes)))
        (loop while (<= (+ pos 8) limit)
              for type = (%autofdo-u32le bytes pos)
              for size = (%autofdo-u16le bytes (+ pos 6))
              do (cond
                   ((and type size (<= 8 size) (<= (+ pos size) limit)
                         (<= type 128))
                    (cond
                      ((= type +perf-record-mmap+)
                       (let ((row (%autofdo-parse-perf-mmap-record bytes pos size)))
                         (when row (push row map))))
                      ((= type +perf-record-sample+)
                       (let ((sample (%autofdo-parse-perf-sample-record bytes pos size)))
                         (when sample
                           (let* ((ip (getf sample :ip))
                                  (fn (%autofdo-map-ip ip map executable-path)))
                             (setf (getf sample :function) fn)
                             (push sample samples)
                             (%autofdo-record-function-sample hotness fn 1))))))
                    (incf pos size))
                   (t (incf pos))))))
    (let ((rows nil))
      (maphash (lambda (k v) (push (cons k v) rows)) hotness)
      (make-autofdo-profile
       :function-hotness (sort rows #'> :key #'cdr)
       :branch-probabilities nil
       :raw-samples (nreverse samples)
       :perf-map (sort map #'< :key #'first)
       :layout-decisions (autofdo-hot-cold-layout-decisions rows)))))

(defun %autofdo-sample-from-line (line)
  "Return (IP COUNT) from a text profile LINE when possible."
  (let ((parts (remove "" (uiop:split-string line :separator '(#\Space #\Tab #\Newline))
                       :test #'string=)))
    (cond
      ;; AutoFDO-ish: "0xIP COUNT" or "IP COUNT".
      ((and (>= (length parts) 2)
            (%autofdo-parse-ip (first parts))
            (%autofdo-parse-int (second parts)))
       (list (%autofdo-parse-ip (first parts)) (%autofdo-parse-int (second parts))))
      ;; perf script-ish: pick the first hexadecimal token as IP, count defaults to 1.
      (t (let ((ip-token (find-if #'%autofdo-hex-token-p parts)))
           (and ip-token (list (%autofdo-parse-ip ip-token) 1)))))))

(defun autofdo-hot-cold-layout-decisions (function-hotness &key (hot-percentile 0.80))
  "Generate hot/cold function layout decisions from FUNCTION-HOTNESS rows."
  (let* ((rows (sort (copy-list function-hotness) #'> :key #'cdr))
         (total (reduce #'+ rows :key #'cdr :initial-value 0))
         (seen 0))
    (loop for (fn . count) in rows
          for hotp = (or (zerop total)
                         (<= (/ (float (incf seen count)) total) hot-percentile)
                         (= count (cdar rows)))
          collect (list :function fn
                        :count count
                        :layout (if hotp :hot :cold)))))

(defun read-autofdo-profile (profile-path &key perf-map-path executable-path)
  "Read PROFILE-PATH and return an AUTOFDO-PROFILE structure.
Supported inputs include AutoFDO address/count rows, `perf script` rows, and a
basic binary perf.data parser for PERF_RECORD_MMAP/PERF_RECORD_SAMPLE."
  (let* ((bytes (%autofdo-read-file-bytes profile-path)))
    (when (%autofdo-binary-profile-p bytes)
      (return-from read-autofdo-profile
        (read-perf-data-binary profile-path
                               :perf-map-path perf-map-path
                               :executable-path executable-path))))
  (let* ((map (read-perf-map (or perf-map-path (perf-map-path))))
          (hotness (make-hash-table :test #'equal))
          (samples nil))
    (when (probe-file profile-path)
      (handler-case
          (with-open-file (in profile-path :direction :input :element-type 'character)
            (loop for line = (read-line in nil nil)
                  while line
                  for sample = (%autofdo-sample-from-line line)
                  when sample
                    do (destructuring-bind (ip count) sample
                          (let ((fn (%autofdo-map-ip ip map executable-path)))
                            (push (list :ip ip :count count :function fn) samples)
                            (%autofdo-record-function-sample hotness fn count)))))
         (error () nil)))
    (let ((rows nil))
      (maphash (lambda (k v) (push (cons k v) rows)) hotness)
      (setf rows (sort rows #'> :key #'cdr))
      (make-autofdo-profile
       :function-hotness rows
       :branch-probabilities nil
       :raw-samples (nreverse samples)
       :perf-map map
       :layout-decisions (autofdo-hot-cold-layout-decisions rows)))))

(defun autofdo-profile->plist (profile)
  "Convert AUTOFDO-PROFILE to the pipeline PGO plist representation."
  (list :format :cl-cc-autofdo-v1
        :function-hotness (autofdo-profile-function-hotness profile)
        :branch-probabilities (autofdo-profile-branch-probabilities profile)
        :raw-samples (autofdo-profile-raw-samples profile)
        :layout-decisions (autofdo-profile-layout-decisions profile)))

(defun load-autofdo-profile-data (profile-path &key perf-map-path executable-path)
  "Read PROFILE-PATH and return pipeline-compatible sample PGO data."
  (autofdo-profile->plist
   (read-autofdo-profile profile-path
                         :perf-map-path perf-map-path
                         :executable-path executable-path)))

(defun autofdo-apply-layout-decisions (instructions profile-data)
  "Apply hot/cold layout decisions when profile data names known labels.

The optimizer's CFG hot/cold pass remains the fallback for branch-local layout.
This pass handles function-entry labels from AutoFDO by moving cold labelled
blocks after hot/unknown blocks while preserving each block's internal order."
  (let ((decisions (and (consp profile-data) (getf profile-data :layout-decisions))))
    (if (null decisions)
        instructions
        (let ((cold (mapcar (lambda (row) (getf row :function))
                            (remove-if-not (lambda (row) (eq (getf row :layout) :cold))
                                           decisions)))
              (blocks nil)
              (current nil))
          (dolist (inst instructions)
            (when (and (typep inst 'cl-cc/vm:vm-label) current)
              (push (nreverse current) blocks)
              (setf current nil))
            (push inst current))
          (when current (push (nreverse current) blocks))
          (let* ((ordered (nreverse blocks))
                 (cold-block-p (lambda (block)
                                 (let ((head (first block)))
                                   (and (typep head 'cl-cc/vm:vm-label)
                                        (member (princ-to-string (cl-cc/vm::vm-name head)) cold
                                                :test #'string=))))))
            (append (mapcan #'copy-list (remove-if cold-block-p ordered))
                    (mapcan #'copy-list (remove-if-not cold-block-p ordered))))))))

(defun autofdo-apply-branch-probabilities (instructions profile-data)
  "Feed sample branch probabilities into the branch prediction representation.
PROFILE-DATA may contain (:BRANCH-PROBABILITIES ((PC . PROB) ...)).  PROB is the
taken probability.  Without branch samples this is a non-transforming pass."
  (let ((probs (and (consp profile-data) (getf profile-data :branch-probabilities))))
    (if probs
        (loop for inst in instructions
              for pc from 0
              for prob = (cdr (assoc pc probs :test #'eql))
              collect (if (and prob (typep inst 'cl-cc/vm:vm-jump-zero))
                          (cl-cc/optimize::make-vm-branch-weighted-jump-zero
                           :reg (cl-cc/vm:vm-reg inst)
                           :label (cl-cc/vm:vm-label-name inst)
                           :branch-weight (if (>= prob 0.5) :likely-taken :not-taken))
                          inst))
        instructions)))

(defun maybe-load-autofdo-profile-data (profile-data)
  "Return PROFILE-DATA, loading AutoFDO/perf text files when PROFILE-DATA is a path."
  (typecase profile-data
    ((or pathname string) (load-autofdo-profile-data profile-data))
    (t profile-data)))
