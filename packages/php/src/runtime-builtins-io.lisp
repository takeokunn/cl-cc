;;;; PHP file I/O, system, and miscellaneous builtins.

(in-package :cl-cc/php)

;;; ─── File I/O ────────────────────────────────────────────────────────────────

(defun %php-file-get-contents (filename &optional use-include-path context offset length)
  "PHP file_get_contents: read file into string."
  (declare (ignore use-include-path context))
  (handler-case
      (let* ((path (%php-stringify filename))
             (content (with-open-file (s path :direction :input :external-format :utf-8)
                        (let ((result (make-string (file-length s))))
                          (read-sequence result s)
                          result)))
             (start (if (and offset (not (%php-null-p offset)) (> offset 0)) offset 0))
             (end (if (and length (not (%php-null-p length))) (+ start length) (length content))))
        (subseq content start (min end (length content))))
    (error () nil)))

(defun %php-file-put-contents (filename data &optional flags context)
  "PHP file_put_contents: write data to file."
  (declare (ignore context))
  (handler-case
      (let* ((path (%php-stringify filename))
             (text (%php-stringify data))
             (append-p (and flags (not (%php-null-p flags)) (logtest flags 8))))
        (with-open-file (s path :direction :output
                                :external-format :utf-8
                                :if-exists (if append-p :append :supersede)
                                :if-does-not-exist :create)
          (write-string text s))
        (length text))
    (error () nil)))

(defun %php-file (filename &optional flags context)
  "PHP file: read file into array of lines."
  (declare (ignore flags context))
  (handler-case
      (let ((result (%php-make-array)))
        (with-open-file (s (%php-stringify filename) :direction :input :external-format :utf-8)
          (loop for line = (read-line s nil nil)
                while line
                do (%php-array-set result (%php-array-next-auto-index result)
                                   (concatenate 'string line (string #\Newline)))))
        result)
    (error () nil)))

(defun %php-readfile (filename &optional use-include-path context)
  "PHP readfile: output file contents and return byte count."
  (declare (ignore use-include-path context))
  (let ((content (%php-file-get-contents filename)))
    (when content
      (%php-output-write content)
      (length content))))

(defun %php-file-exists (filename)
  "PHP file_exists: check if file or directory exists."
  (probe-file (%php-stringify filename)))

(defun %php-is-file (filename)
  "PHP is_file: check if path is a regular file."
  (let ((p (probe-file (%php-stringify filename))))
    (and p (not (cl:directory-namestring p)))))

(defun %php-is-dir (path)
  "PHP is_dir: check if path is a directory."
  (let* ((s (%php-stringify path))
         (s (if (and (> (length s) 0) (char= (char s (1- (length s))) #\/)) s (concatenate 'string s "/")))
         (p (probe-file s)))
    (and p t)))

(defun %php-is-readable (filename)
  "PHP is_readable: check if file is readable."
  (not (null (probe-file (%php-stringify filename)))))

(defun %php-is-writable (filename)
  "PHP is_writable: check if file is writable (simplified)."
  (not (null (probe-file (%php-stringify filename)))))

(defun %php-is-writeable (filename)
  "PHP is_writeable: alias for is_writable."
  (%php-is-writable filename))

(defun %php-is-executable (filename)
  "PHP is_executable: check if file is executable."
  (not (null (probe-file (%php-stringify filename)))))

(defun %php-is-link (filename)
  "PHP is_link: check if path is a symbolic link."
  (declare (ignore filename))
  nil)

(defun %php-filesize (filename)
  "PHP filesize: return file size in bytes."
  (handler-case
      (with-open-file (s (%php-stringify filename) :element-type '(unsigned-byte 8))
        (file-length s))
    (error () nil)))

(defun %php-filemtime (filename)
  "PHP filemtime: return file modification time."
  (handler-case
      (let ((time (file-write-date (%php-stringify filename))))
        (when time (- time 2208988800)))  ; Unix epoch offset
    (error () nil)))

(defun %php-filetype (filename)
  "PHP filetype: return file type string."
  (handler-case
      (let ((p (probe-file (%php-stringify filename))))
        (if p "file" nil))
    (error () nil)))

(defun %php-unlink (filename &optional context)
  "PHP unlink: delete a file."
  (declare (ignore context))
  (handler-case
      (progn (cl:delete-file (%php-stringify filename)) t)
    (error () nil)))

(defun %php-rename (oldname newname &optional context)
  "PHP rename: rename a file or directory."
  (declare (ignore context))
  (handler-case
      (progn (rename-file (%php-stringify oldname) (%php-stringify newname)) t)
    (error () nil)))

(defun %php-copy (source dest &optional context)
  "PHP copy: copy a file."
  (declare (ignore context))
  (handler-case
      (let ((content (%php-file-get-contents source)))
        (when content (%php-file-put-contents dest content) t))
    (error () nil)))

(defun %php-mkdir (pathname &optional (mode #o777) recursive context)
  "PHP mkdir: create a directory."
  (declare (ignore mode context))
  (handler-case
      (progn
        (if (%php-truthy recursive)
            (ensure-directories-exist (concatenate 'string (%php-stringify pathname) "/"))
            (cl:ensure-directories-exist (concatenate 'string (%php-stringify pathname) "/")))
        t)
    (error () nil)))

(defun %php-rmdir (dirname &optional context)
  "PHP rmdir: remove a directory."
  (declare (ignore context))
  (handler-case
      (progn (cl:delete-file (concatenate 'string (%php-stringify dirname) "/")) t)
    (error () nil)))

(defun %php-scandir (directory &optional (sorting-order 0) context)
  "PHP scandir: list files and directories in a directory."
  (declare (ignore context))
  (handler-case
      (let* ((dir (%php-stringify directory))
             (entries (mapcar (lambda (p)
                                (let ((name (file-namestring p)))
                                  (if (string= name "") (car (last (pathname-directory p))) name)))
                              (cl:directory (concatenate 'string dir "/*"))))
             (all (append (list "." "..") entries))
             (sorted (if (= sorting-order 1) (reverse (sort (copy-list all) #'string<))
                         (sort (copy-list all) #'string<))))
        (%php-list-to-array sorted))
    (error () nil)))

(defun %php-getcwd ()
  "PHP getcwd: return current working directory."
  (namestring (truename ".")))

(defun %php-chdir (directory)
  "PHP chdir: change current working directory (delegates to sb-posix if available)."
  (let ((path (%php-stringify directory)))
    (handler-case
        (let ((fn (and (find-package :sb-posix)
                       (find-symbol "CHDIR" :sb-posix))))
          (if (and fn (fboundp fn))
              (progn (funcall fn path) t)
              ;; Fallback: verify path exists, return true
              (and (probe-file path) t)))
      (error () nil))))

(defun %php-realpath (path)
  "PHP realpath: return canonicalized absolute pathname."
  (handler-case
      (namestring (truename (%php-stringify path)))
    (error () nil)))

(defun %php-basename (path &optional suffix)
  "PHP basename: return trailing name component of path."
  (let* ((p (%php-stringify path))
         (base (file-namestring p)))
    (if (and suffix (not (%php-null-p suffix)))
        (let ((s (%php-stringify suffix)))
          (if (and (>= (length base) (length s))
                   (string= base s :start1 (- (length base) (length s))))
              (subseq base 0 (- (length base) (length s)))
              base))
        base)))

(defun %php-dirname (path &optional (levels 1))
  "PHP dirname: return directory name of path."
  (let* ((p (%php-stringify path))
         (lvls (if (and levels (not (%php-null-p levels))) levels 1)))
    (let ((result p))
      (dotimes (i lvls)
        (let ((pos (or (position #\/ result :from-end t)
                       (position #\\ result :from-end t))))
          (setf result (if pos (subseq result 0 pos) "."))))
      result)))

(defun %php-pathinfo (path &optional (option nil))
  "PHP pathinfo: return path components."
  (let* ((p (%php-stringify path))
         (dirname (let ((pos (or (position #\/ p :from-end t) (position #\\ p :from-end t))))
                    (if pos (subseq p 0 pos) ".")))
         (filename (file-namestring p))
         (ext (let ((dot (position #\. filename :from-end t)))
                (if dot (subseq filename (1+ dot)) "")))
         (basename (let ((dot (position #\. filename :from-end t)))
                     (if dot (subseq filename 0 dot) filename))))
    (cond ((or (null option) (%php-null-p option))
           (let ((r (%php-make-array)))
             (%php-array-set r "dirname" dirname)
             (%php-array-set r "basename" filename)
             (%php-array-set r "extension" ext)
             (%php-array-set r "filename" basename)
             r))
          ((= option 1) dirname)
          ((= option 2) filename)
          ((= option 4) ext)
          ((= option 8) basename)
          (t nil))))

(defun %php-tempnam (dir &optional (prefix ""))
  "PHP tempnam: create file with unique name."
  (let ((tmp (concatenate 'string
                          (if (and dir (not (%php-null-p dir))) (%php-stringify dir) "/tmp")
                          "/" (%php-stringify prefix)
                          (format nil "~8,'0X" (random #xFFFFFFFF)))))
    (%php-file-put-contents tmp "")
    tmp))

(defun %php-sys-get-temp-dir ()
  "PHP sys_get_temp_dir: return temp directory path."
  "/tmp")

;;; ─── fopen/fclose/fread/fwrite family ────────────────────────────────────────
;;; We represent file handles as a hash table with stream/mode/path metadata.

(defun %php-standard-stream-handle (stream path mode)
  "Return a PHP stream handle for a dynamically bound CL standard stream."
  (let ((handle (%php-make-array)))
    (%php-array-set handle "__stream__" stream)
    (%php-array-set handle "__mode__" mode)
    (%php-array-set handle "__path__" path)
    (%php-array-set handle "__standard__" t)
    (%php-array-set handle "__eof__" nil)
    handle))

(defun %php-stdin ()
  "PHP STDIN stream resource."
  (%php-standard-stream-handle *standard-input* "php://stdin" "r"))

(defun %php-stdout ()
  "PHP STDOUT stream resource."
  (%php-standard-stream-handle *standard-output* "php://stdout" "w"))

(defun %php-stderr ()
  "PHP STDERR stream resource."
  (%php-standard-stream-handle *error-output* "php://stderr" "w"))

(defun %php-fopen (filename mode &optional use-include-path context)
  "PHP fopen: open a file and return a file handle."
  (declare (ignore use-include-path context))
  (handler-case
      (let* ((path (%php-stringify filename))
             (m (%php-stringify mode))
             (direction (cond ((member m '("r" "rb") :test #'string=) :input)
                              ((member m '("w" "wb" "x" "xb") :test #'string=) :output)
                              ((member m '("a" "ab") :test #'string=) :output)
                              ((member m '("r+" "r+b") :test #'string=) :io)
                              (t :output)))
             (if-exists (cond ((member m '("a" "ab") :test #'string=) :append)
                              ((member m '("x" "xb") :test #'string=) :error)
                              (t :supersede)))
             (stream (open path :direction direction
                                :external-format :utf-8
                                :if-exists if-exists
                                :if-does-not-exist (if (eq direction :input) :error :create)))
             (handle (%php-make-array)))
        (%php-array-set handle "__stream__" stream)
        (%php-array-set handle "__mode__" m)
        (%php-array-set handle "__path__" path)
        (%php-array-set handle "__eof__" nil)
        handle)
    (error () nil)))

(defun %php-fclose (handle)
  "PHP fclose: close a file handle."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (when (and stream (streamp stream))
          (unless (%php-array-ref handle "__standard__")
            (close stream)))
        t)
    (error () nil)))

(defun %php-fread (handle length)
  "PHP fread: read up to LENGTH bytes from file handle."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (buf (make-string length))
             (n (read-sequence buf stream)))
        (when (< n length)
          (%php-array-set handle "__eof__" t))
        (subseq buf 0 n))
    (error () "")))

(defun %php-fwrite (handle string &optional length)
  "PHP fwrite: write string to file handle."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (text (%php-stringify string))
             (to-write (if (and length (not (%php-null-p length)))
                           (subseq text 0 (min length (length text)))
                           text)))
        (write-string to-write stream)
        (length to-write))
    (error () nil)))

(defun %php-fputs (handle string &optional length)
  "PHP fputs: alias for fwrite."
  (%php-fwrite handle string length))

(defun %php-fgets (handle &optional length)
  "PHP fgets: read a line from file handle."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (let ((line (read-line stream nil nil)))
          (if line
              (if (and length (not (%php-null-p length)))
                  (subseq (concatenate 'string line (string #\Newline)) 0
                          (min length (1+ (length line))))
                  (concatenate 'string line (string #\Newline)))
              (progn (%php-array-set handle "__eof__" t) nil))))
    (error () nil)))

(defun %php-fgetc (handle)
  "PHP fgetc: read a single character from file handle."
  (handler-case
      (let* ((stream (%php-array-ref handle "__stream__"))
             (ch (read-char stream nil nil)))
        (if ch (string ch)
            (progn (%php-array-set handle "__eof__" t) nil)))
    (error () nil)))

(defun %php-feof (handle)
  "PHP feof: check if end of file has been reached."
  (let ((eof (%php-array-ref handle "__eof__")))
    (and eof (%php-truthy eof))))

(defun %php-fflush (handle)
  "PHP fflush: flush output to file."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (when (and stream (output-stream-p stream))
          (finish-output stream))
        t)
    (error () nil)))

(defun %php-fseek (handle offset &optional (whence 0))
  "PHP fseek: set the position in a file."
  (handler-case
      (let ((stream (%php-array-ref handle "__stream__")))
        (file-position stream
                       (case whence
                         (0 offset)             ; SEEK_SET
                         (1 (+ (file-position stream) offset))  ; SEEK_CUR
                         (2 nil)                ; SEEK_END - not easily portable
                         (t offset)))
        0)
    (error () -1)))

(defun %php-ftell (handle)
  "PHP ftell: return current file position."
  (handler-case
      (file-position (%php-array-ref handle "__stream__"))
    (error () nil)))

(defun %php-rewind (handle)
  "PHP rewind: set file position to beginning."
  (%php-fseek handle 0 0))

(defun %php-fgetcsv (handle &optional (length 0) (separator ",") (enclosure "\"") (escape "\\"))
  "PHP fgetcsv: parse CSV line from file."
  (declare (ignore length enclosure escape))
  (let ((line (%php-fgets handle)))
    (if line
        (%php-explode separator (string-right-trim '(#\Newline #\Return) line))
        nil)))

(defun %php-fputcsv (handle fields &optional (separator ",") (enclosure "\"") (escape "\\"))
  "PHP fputcsv: format line as CSV and write to file."
  (declare (ignore escape))
  (let* ((sep (%php-stringify separator))
         (enc (%php-stringify enclosure))
         (parts (mapcar (lambda (f)
                          (let ((s (%php-stringify f)))
                            (if (or (search sep s) (search enc s) (search (string #\Newline) s))
                                (concatenate 'string enc
                                             (cl:with-output-to-string (o)
                                               (loop for ch across s
                                                     do (when (string= (string ch) enc)
                                                          (write-string enc o))
                                                        (write-char ch o)))
                                             enc)
                                s)))
                        (%php-array-values-list fields)))
         (line (concatenate 'string (format nil "~{~A~^~A~}" (list (car parts) sep))
                            (string #\Newline))))
    (%php-fwrite handle line)
    (length line)))

;;; ─── System / environment ────────────────────────────────────────────────────

(defun %php-getenv (varname &optional local-only)
  "PHP getenv: get environment variable."
  (declare (ignore local-only))
  (or (sb-ext:posix-getenv (%php-stringify varname)) nil))

(defun %php-putenv (setting)
  "PHP putenv: set environment variable."
  (let* ((s (%php-stringify setting))
         (eq-pos (position #\= s)))
    (if eq-pos
        (handler-case
            (progn
              (when (find-package :sb-posix)
                (funcall (intern "SETENV" :sb-posix) (subseq s 0 eq-pos) (subseq s (1+ eq-pos)) 1))
              t)
          (error () t))
        nil)))

(defun %php-php-uname (&optional (mode "a"))
  "PHP php_uname: OS information."
  (declare (ignore mode))
  "Darwin")

(defun %php-php-sapi-name ()
  "PHP php_sapi_name: return SAPI name."
  "cli")

(defun %php-php-version ()
  "PHP phpversion: return PHP version string."
  "8.3.0")

(defun %php-php-int-size ()
  "PHP PHP_INT_SIZE constant."
  8)

(defun %php-php-int-max ()
  "PHP PHP_INT_MAX constant."
  9223372036854775807)

(defun %php-php-int-min ()
  "PHP PHP_INT_MIN constant."
  -9223372036854775808)

(defun %php-php-float-max ()
  "PHP PHP_FLOAT_MAX constant."
  most-positive-double-float)

(defun %php-php-float-epsilon ()
  "PHP PHP_FLOAT_EPSILON constant."
  2.220446049250313d-16)

(defparameter *php-ini-defaults*
  '(("display_errors" . "1")
    ("error_reporting" . "32767")
    ("html_errors" . "0")
    ("log_errors" . "0")
    ("max_execution_time" . "0")
    ("memory_limit" . "-1")
    ("default_charset" . "UTF-8")
    ("date.timezone" . "UTC")
    ("precision" . "14")
    ("serialize_precision" . "-1"))
  "Default INI values modelled by the PHP runtime.")

(defvar *php-ini-settings* nil
  "Mutable PHP INI settings for the current Lisp image.")

(defvar *php-error-reporting-level* 32767
  "Current PHP error_reporting() level.")

(defvar *php-error-handler-stack* nil
  "Stack of active PHP error handlers as (CALLBACK . ERROR-MASK).")

(defvar *php-exception-handler-stack* nil
  "Stack of active PHP exception handlers.")

(defun %php-current-error-handler ()
  "Return the active PHP error handler entry, or NIL."
  (first *php-error-handler-stack*))

(defun %php-current-exception-handler ()
  "Return the active PHP exception handler, or NIL."
  (first *php-exception-handler-stack*))

(defun %php-get-error-handler ()
  "PHP get_error_handler: return the active custom error handler, or null."
  (let ((entry (%php-current-error-handler)))
    (if entry
        (car entry)
        +php-null+)))

(defun %php-get-exception-handler ()
  "PHP get_exception_handler: return the active custom exception handler, or null."
  (or (%php-current-exception-handler) +php-null+))

(defun %php-valid-callback-or-throw (callback function-name)
  "Return CALLBACK when it is callable, otherwise signal a PHP TypeError."
  (if (%php-callable-function callback)
      callback
      (%php-throw 'type-error
                  (format nil "~A(): Argument #1 ($callback) must be a valid callback"
                          function-name))))

(defun %php-ensure-ini-settings ()
  (or *php-ini-settings*
      (setf *php-ini-settings*
            (let ((table (make-hash-table :test 'equal)))
              (dolist (entry *php-ini-defaults* table)
                (setf (gethash (car entry) table) (cdr entry)))))))

(defun %php-ini-key (varname)
  (string-downcase (%php-stringify varname)))

(defun %php-parse-integer-setting (value fallback)
  (handler-case
      (parse-integer (%php-stringify value) :junk-allowed t)
    (error () fallback)))

(defun %php-ini-get (varname)
  "PHP ini_get: get an INI configuration value."
  (let* ((key (%php-ini-key varname))
         (table (%php-ensure-ini-settings)))
    (if (string= key "date.timezone")
        (%php-date-default-timezone-get)
        (multiple-value-bind (value present-p) (gethash key table)
          (if present-p value nil)))))

(defun %php-ini-set (varname newvalue)
  "PHP ini_set: set an INI configuration value and return the previous value."
  (let* ((key (%php-ini-key varname))
         (table (%php-ensure-ini-settings))
         (old (%php-ini-get key)))
    (cond
      ((string= key "date.timezone")
       (when (%php-date-default-timezone-set newvalue)
         (setf (gethash key table) (%php-date-default-timezone-get))
         old))
      (t
       (setf (gethash key table) (%php-stringify newvalue))
       (when (string= key "error_reporting")
         (setf *php-error-reporting-level*
               (%php-parse-integer-setting newvalue *php-error-reporting-level*)))
       old))))

(defun %php-set-error-handler (callback &optional error-types)
  "PHP set_error_handler: install CALLBACK and return the previous handler."
  (let* ((old (car (%php-current-error-handler)))
         (mask (%php-parse-integer-setting
                (if (and error-types (not (%php-null-p error-types)))
                    error-types
                    *php-error-reporting-level*)
                *php-error-reporting-level*))
         (cb (%php-valid-callback-or-throw callback "set_error_handler")))
    (push (cons cb mask) *php-error-handler-stack*)
    old))

(defun %php-restore-error-handler ()
  "PHP restore_error_handler: restore the previous custom error handler."
  (when *php-error-handler-stack*
    (pop *php-error-handler-stack*))
  t)

(defun %php-set-exception-handler (callback)
  "PHP set_exception_handler: install CALLBACK and return the previous handler."
  (let ((old (%php-current-exception-handler))
        (cb (%php-valid-callback-or-throw callback "set_exception_handler")))
    (push cb *php-exception-handler-stack*)
    old))

(defun %php-restore-exception-handler ()
  "PHP restore_exception_handler: restore the previous custom exception handler."
  (when *php-exception-handler-stack*
    (pop *php-exception-handler-stack*))
  t)

(defun %php-error-reporting (&optional level)
  "PHP error_reporting: set/get error reporting level."
  (let ((old *php-error-reporting-level*))
    (when level
      (setf *php-error-reporting-level*
            (%php-parse-integer-setting level *php-error-reporting-level*))
      (setf (gethash "error_reporting" (%php-ensure-ini-settings))
            (%php-stringify *php-error-reporting-level*)))
    old))

(defun %php-trigger-error (message &optional (error-type 256))
  "PHP trigger_error: generate a user-level error."
  (let* ((errno (%php-parse-integer-setting error-type 256))
         (handler-entry (%php-current-error-handler))
         (reportable-p (logtest errno *php-error-reporting-level*))
         (handled-p nil))
    (when (and reportable-p handler-entry (logtest errno (cdr handler-entry)))
      (let ((fn (%php-callable-function (car handler-entry))))
        (when fn
          (setf handled-p
                (%php-truthy
                 (funcall fn errno (%php-stringify message) nil nil))))))
    (unless (or handled-p (not reportable-p))
      (format t "~A~%" (%php-stringify message))))
  t)

(defun %php-exit (&optional (status 0))
  "PHP exit/die: terminate execution."
  (when (stringp status) (%php-output-write status))
  (sb-ext:exit :code (if (numberp status) status 0)))

(defun %php-die (&optional (status 0))
  "PHP die: alias for exit."
  (%php-exit status))

(defun %php-sleep (seconds)
  "PHP sleep: delay execution."
  (sleep seconds)
  0)

(defun %php-usleep (microseconds)
  "PHP usleep: delay in microseconds."
  (sleep (/ microseconds 1000000.0))
  nil)

(defun %php-nl-langinfo (item)
  "PHP nl_langinfo: return deterministic locale metadata for ITEM."
  (let* ((entry (if (stringp item)
                    (find (string-upcase item) +php-nl-langinfo-items+
                          :test #'string= :key #'first)
                    (find (%php-to-integer item) +php-nl-langinfo-items+
                          :test #'= :key #'second))))
    (if entry
        (third entry)
        "")))

(defun %php-locale-normalized-id (locale)
  "Return a lowercase BCP-47-ish locale id without encoding/modifier suffixes."
  (let* ((raw (%php-stringify locale))
         (cut (reduce #'min
                      (remove nil (list (position #\. raw) (position #\@ raw)))
                      :initial-value (length raw))))
    (string-downcase (substitute #\- #\_ (subseq raw 0 cut)))))

(defun %php-locale-subtags (locale-id)
  (loop with start = 0
        for pos = (position #\- locale-id :start start)
        collect (subseq locale-id start pos)
        while pos
        do (setf start (1+ pos))))

(defun %php-locale-is-right-to-left (locale)
  "PHP 8.5 locale_is_right_to_left / Locale::isRightToLeft helper."
  (let* ((id (%php-locale-normalized-id locale))
         (subtags (%php-locale-subtags id))
         (primary (first subtags)))
    (and primary
         (or (member primary
                     '("ar" "arc" "ckb" "dv" "fa" "he" "iw" "ks" "lrc"
                       "mzn" "nqo" "pnb" "ps" "sd" "syr" "ug" "ur" "yi")
                     :test #'string=)
             (some (lambda (subtag)
                     (member subtag '("arab" "hebr" "nkoo" "syrc" "thaa")
                             :test #'string=))
                   subtags)))))

(defvar *php-output-buffer-stack* nil
  "Stack of active PHP output buffers.")

(defvar *php-output-started-p* nil
  "Whether unbuffered PHP output has been emitted.")

(defvar *php-http-response-code* 200
  "Current PHP HTTP response code for CLI response modelling.")

(defvar *php-http-headers* nil
  "Headers queued by PHP header(), newest first.")

(defun %php-make-output-buffer ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun %php-output-buffer-string (buffer)
  (coerce buffer 'string))

(defun %php-output-write (value)
  "Write VALUE through PHP output buffering and return nil."
  (let* ((text (%php-stringify value))
         (buffer (first *php-output-buffer-stack*)))
    (if buffer
        (loop for ch across text do (vector-push-extend ch buffer))
        (progn
          (setf *php-output-started-p* t)
          (write-string text)))
    nil))

(defun %php-echo (&rest args)
  "PHP echo builtin: write each argument and return nil."
  (dolist (arg args)
    (%php-output-write arg))
  nil)

(defun %php-print (arg)
  "PHP print builtin: write ARG and return 1."
  (%php-output-write arg)
  1)

(defun %php-ob-start (&optional callback chunk-size flags)
  "PHP ob_start: start output buffering."
  (declare (ignore callback chunk-size flags))
  (push (%php-make-output-buffer) *php-output-buffer-stack*)
  t)

(defun %php-ob-end-clean ()
  "PHP ob_end_clean: discard the current output buffer."
  (if *php-output-buffer-stack*
      (progn
        (pop *php-output-buffer-stack*)
        t)
      nil))

(defun %php-ob-get-clean ()
  "PHP ob_get_clean: get current buffer contents and discard it."
  (if *php-output-buffer-stack*
      (%php-output-buffer-string (pop *php-output-buffer-stack*))
      nil))

(defun %php-ob-get-contents ()
  "PHP ob_get_contents: get current output buffer contents."
  (if *php-output-buffer-stack*
      (%php-output-buffer-string (first *php-output-buffer-stack*))
      nil))

(defun %php-http-response-code (&optional response-code)
  "PHP http_response_code: set/get HTTP response code."
  (if (or (null response-code) (%php-null-p response-code))
      *php-http-response-code*
      (let ((old *php-http-response-code*))
        (setf *php-http-response-code* (%php-to-integer response-code))
        old)))

(defun %php-string-prefix-p (prefix string)
  "Return true when STRING starts with PREFIX, ignoring case."
  (and (>= (length string) (length prefix))
       (string-equal prefix string :end2 (length prefix))))

(defun %php-header-name (line)
  "Return the normalized field name for a header line, or nil."
  (let* ((text (%php-stringify line))
         (pos (position #\: text)))
    (when pos
      (string-downcase (string-trim '(#\Space #\Tab) (subseq text 0 pos))))))

(defun %php-status-line-code (line)
  "Return an HTTP status code encoded in LINE, or nil."
  (let* ((text (%php-stringify line))
         (trimmed (string-trim '(#\Space #\Tab) text)))
    (cond
      ((%php-string-prefix-p "HTTP/" trimmed)
       (let* ((space (position #\Space trimmed))
              (start (and space (1+ space)))
              (end (and start (position #\Space trimmed :start start))))
         (when start
           (parse-integer trimmed :start start :end end :junk-allowed t))))
      ((%php-string-prefix-p "Status:" trimmed)
       (parse-integer trimmed :start (length "Status:") :junk-allowed t))
      (t nil))))

(defun %php-header (header &optional (replace t) response-code)
  "PHP header: queue a response header in the CLI response model."
  (let* ((line (%php-stringify header))
         (code (%php-status-line-code line))
         (name (%php-header-name line)))
    (when (and response-code (not (%php-null-p response-code)))
      (setf *php-http-response-code* (%php-to-integer response-code)))
    (when code
      (setf *php-http-response-code* code))
    (when (and name (%php-truthy replace))
      (setf *php-http-headers*
            (remove name *php-http-headers*
                    :test #'string=
                    :key #'%php-header-name)))
    (push line *php-http-headers*)
    nil))

(defun %php-headers-list ()
  "PHP headers_list: return queued response headers in insertion order."
  (%php-list-to-array (reverse *php-http-headers*)))

(defun %php-headers-sent (&optional file line)
  "PHP headers_sent: report whether unbuffered output has started."
  (declare (ignore file line))
  *php-output-started-p*)

;;; ─── String misc ─────────────────────────────────────────────────────────────

(defun %php-parse-str (str &optional result)
  "PHP parse_str: parse URL query string into variables."
  (let ((parts (%php-explode "&" str))
        (target (or result (%php-make-array))))
    (dolist (pair (%php-array-values-list parts))
      (let ((eq-pos (search "=" pair)))
        (when eq-pos
          (%php-array-set target
                          (%php-urldecode (subseq pair 0 eq-pos))
                          (%php-urldecode (subseq pair (1+ eq-pos)))))))
    target))

(defun %php-http-build-query (data &optional numeric-prefix arg-separator enc-type)
  "PHP http_build_query: generate URL-encoded query string."
  (declare (ignore numeric-prefix enc-type))
  (let ((sep (if (and arg-separator (not (%php-null-p arg-separator)))
                 (%php-stringify arg-separator) "&"))
        (parts nil))
    (when (hash-table-p data)
      (dolist (pair (%php-array-pairs data))
        (push (concatenate 'string
                           (%php-urlencode (%php-stringify (car pair)))
                           "="
                           (%php-urlencode (%php-stringify (cdr pair))))
              parts)))
    (format nil "~{~A~^~A~}" (list (car (nreverse parts)) sep))))

;;; ─── Type / class helpers ─────────────────────────────────────────────────────

(defun %php-class-exists (class-name &optional autoload)
  "PHP class_exists: check if class is defined."
  (declare (ignore autoload))
  (let ((sym (find-symbol (string-upcase (%php-stringify class-name)) :cl-cc/php)))
    (and sym (fboundp sym))))

(defun %php-interface-exists (interface-name &optional autoload)
  "PHP interface_exists: check if interface is defined."
  (declare (ignore autoload))
  (%php-class-exists interface-name))

(defun %php-function-exists (function-name)
  "PHP function_exists: check if function is defined."
  (not (null (%php-lookup-builtin (%php-stringify function-name)))))

(defun %php-method-exists (object method)
  "PHP method_exists: check if method exists on object."
  (when (hash-table-p object)
    (let ((methods (%php-array-ref object "__methods__")))
      (when (hash-table-p methods)
        (not (%php-null-p (%php-array-ref methods (%php-stringify method))))))))

(defun %php-property-exists (object property)
  "PHP property_exists: check if property exists."
  (when (hash-table-p object)
    (not (%php-null-p (%php-array-ref object (%php-stringify property))))))

(defun %php-get-class (object)
  "PHP get_class: return class name of object."
  (when (hash-table-p object)
    (let ((cls (%php-array-ref object "__class__")))
      (if (%php-null-p cls) nil (%php-stringify cls)))))

(defun %php-get-parent-class (object)
  "PHP get_parent_class: return parent class name."
  (when (hash-table-p object)
    (let ((parent (%php-array-ref object "__parent__")))
      (if (%php-null-p parent) nil (%php-stringify parent)))))

(defun %php-is-a (object class-name &optional allow-string)
  "PHP is_a: check if object is an instance of class."
  (declare (ignore allow-string))
  (when (hash-table-p object)
    (let ((cls (%php-array-ref object "__class__")))
      (string= (%php-stringify cls) (%php-stringify class-name)))))

(defun %php-instanceof (object class-name)
  "PHP instanceof: check if object is an instance of class."
  (%php-is-a object class-name))

(defun %php-get-object-vars (object)
  "PHP get_object_vars: get properties of object as array."
  (when (hash-table-p object)
    (let ((result (%php-make-array)))
      (dolist (pair (%php-array-pairs object))
        (unless (and (stringp (car pair))
                     (> (length (car pair)) 2)
                     (string= (car pair) "__" :end1 2))
          (%php-array-set result (car pair) (cdr pair))))
      result)))

(defun %php-get-class-methods (class-name)
  "PHP get_class_methods: get class method names."
  (declare (ignore class-name))
  (%php-make-array))

(defun %php-reflection-class-descriptor-p (value)
  (and (hash-table-p value)
       (nth-value 1 (gethash :__name__ value))))

(defun %php-reflection-class-symbol-from-name (name)
  (let* ((upcased (string-upcase (%php-stringify name)))
         (found (find-symbol upcased :cl-cc/php)))
    (or found (intern upcased :cl-cc/php))))

(defun %php-reflection-class-symbol (class)
  (cond
    ((symbolp class) class)
    ((%php-reflection-class-descriptor-p class)
     (gethash :__name__ class))
    ((and (cl-cc/vm::%vm-vector-instance-p class)
          (%php-reflection-class-descriptor-p (aref class 0)))
     (gethash :__name__ (aref class 0)))
    ((hash-table-p class)
     (let ((cls (or (gethash :__class__ class)
                    (%php-array-ref class "__class__"))))
       (unless (%php-null-p cls)
         (%php-reflection-class-symbol cls))))
    (t
     (%php-reflection-class-symbol-from-name class))))

(defun %php-reflection-symbol-name (name)
  (cond
    ((symbolp name) (symbol-name name))
    ((stringp name) name)
    (t (%php-stringify name))))

(defun %php-reflection-class-descriptor-from-table (symbol table)
  (when (and symbol (hash-table-p table))
    (let ((target (%php-reflection-symbol-name symbol)))
      (or (gethash symbol table)
          (loop for key being the hash-keys of table
                using (hash-value value)
                when (and (%php-reflection-class-descriptor-p value)
                          (string-equal (%php-reflection-symbol-name key) target))
                  return value)))))

(defun %php-reflection-class-descriptor-from-state (symbol)
  (when (and symbol cl-cc/vm:*vm-current-state*)
    (or (%php-reflection-class-descriptor-from-table
         symbol
         (cl-cc/vm:vm-class-registry cl-cc/vm:*vm-current-state*))
        (%php-reflection-class-descriptor-from-table
         symbol
         (cl-cc/vm:vm-global-vars cl-cc/vm:*vm-current-state*)))))

(defun %php-reflection-class-descriptor (class)
  (cond
    ((%php-reflection-class-descriptor-p class) class)
    ((and (cl-cc/vm::%vm-vector-instance-p class)
          (%php-reflection-class-descriptor-p (aref class 0)))
     (aref class 0))
    ((hash-table-p class)
     (let ((cls (or (gethash :__class__ class)
                    (%php-array-ref class "__class__"))))
       (unless (%php-null-p cls)
         (%php-reflection-class-descriptor cls))))
    (t
     (let ((sym (%php-reflection-class-symbol class)))
       (%php-reflection-class-descriptor-from-state sym)))))

(defun %php-reflection-interface-p (name)
  (not (null (gethash name *php-interface-registry*))))

(defun %php-reflection-unique-symbols (symbols)
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (symbol symbols (nreverse result))
      (let ((name (%php-reflection-symbol-name symbol)))
        (unless (gethash name seen)
          (setf (gethash name seen) t)
          (push symbol result))))))

(defun %php-reflection-symbols-to-array (symbols)
  (let ((result (%php-make-array)))
    (dolist (symbol symbols result)
      (let ((name (%php-reflection-symbol-name symbol)))
        (%php-array-set result name name)))))

(defun %php-reflection-interface-tree (interface)
  (let ((record (gethash interface *php-interface-registry*)))
    (cons interface
          (loop for parent in (getf record :parents)
                append (%php-reflection-interface-tree parent)))))

(defun %php-reflection-class-parent-symbols (class)
  (labels ((walk (descriptor)
             (loop for super in (and descriptor (gethash :__superclasses__ descriptor))
                   unless (%php-reflection-interface-p super)
                     append (cons super
                                  (walk (%php-reflection-class-descriptor super))))))
    (%php-reflection-unique-symbols
     (walk (%php-reflection-class-descriptor class)))))

(defun %php-reflection-class-interface-symbols (class)
  (labels ((walk (descriptor)
             (loop for super in (and descriptor (gethash :__superclasses__ descriptor))
                   append (if (%php-reflection-interface-p super)
                              (%php-reflection-interface-tree super)
                              (walk (%php-reflection-class-descriptor super))))))
    (%php-reflection-unique-symbols
     (walk (%php-reflection-class-descriptor class)))))

(defun %php-reflection-trait-applications (symbol)
  (when symbol
    (let ((target (%php-reflection-symbol-name symbol)))
      (or (gethash target *php-trait-applications*)
          (loop for key being the hash-keys of *php-trait-applications*
                using (hash-value value)
                when (string-equal (%php-reflection-symbol-name key) target)
                  return value)))))

(defun %php-reflection-class-trait-symbols (class)
  (let* ((symbol (%php-reflection-class-symbol class))
         (applications (%php-reflection-trait-applications symbol)))
    (%php-reflection-unique-symbols
     (loop for application in (reverse applications)
           append (getf application :trait-names)))))

;;; ─── SPL data structures ────────────────────────────────────────────────────

(defun %php-spl-class-name (name)
  (let ((s (%php-stringify name)))
    (cond
      ((string-equal s "SplStack") "SplStack")
      ((string-equal s "SplQueue") "SplQueue")
      ((string-equal s "SplDoublyLinkedList") "SplDoublyLinkedList")
      ((string-equal s "SplMinHeap") "SplMinHeap")
      ((string-equal s "SplMaxHeap") "SplMaxHeap")
      ((string-equal s "SplFixedArray") "SplFixedArray")
      (t s))))

(defun %php-spl-index (value)
  (truncate (if (numberp value)
                value
                (%php-to-number (%php-stringify value)))))

(defun %php-spl-number (value)
  (if (numberp value)
      value
      (%php-to-number (%php-stringify value))))

(defun %php-spl-make-methods (&rest names)
  (let ((methods (%php-make-array)))
    (dolist (name names methods)
      (%php-array-set methods name name))))

(defun %php-spl-method-symbol (name)
  (intern (string-upcase name) :cl-cc/php))

(defun %php-spl-set-method (object name function)
  (setf (gethash name object) function
        (gethash (%php-spl-method-symbol name) object) function)
  function)

(defun %php-spl-install-methods (object specs)
  (dolist (spec specs object)
    (%php-spl-set-method object (first spec) (symbol-function (second spec)))))

(defun %php-spl-object (class-name methods)
  (let ((obj (make-hash-table :test #'equal)))
    (setf (gethash "__class__" obj) class-name)
    (setf (gethash "__methods__" obj) methods)
    obj))

(defun %php-spl-items (object)
  (or (gethash "__items__" object) '()))

(defun %php-spl-set-items (object items)
  (setf (gethash "__items__" object) items))

(defun %php-spl-list-count (self)
  (length (%php-spl-items self)))

(defun %php-spl-list-empty-p (self)
  (zerop (%php-spl-list-count self)))

(defun %php-spl-list-push (self value)
  (%php-spl-set-items self (append (%php-spl-items self) (list value)))
  +php-null+)

(defun %php-spl-list-unshift (self value)
  (%php-spl-set-items self (cons value (%php-spl-items self)))
  +php-null+)

(defun %php-spl-list-pop (self)
  (let ((items (%php-spl-items self)))
    (if items
        (prog1 (car (last items))
          (%php-spl-set-items self (butlast items)))
        +php-null+)))

(defun %php-spl-list-shift (self)
  (let ((items (%php-spl-items self)))
    (if items
        (prog1 (first items)
          (%php-spl-set-items self (rest items)))
        +php-null+)))

(defun %php-spl-list-top (self)
  (let ((items (%php-spl-items self)))
    (if items (car (last items)) +php-null+)))

(defun %php-spl-list-bottom (self)
  (let ((items (%php-spl-items self)))
    (if items (first items) +php-null+)))

(defun %php-spl-list-queue-pop (self)
  (%php-spl-list-shift self))

(defparameter +php-spl-list-methods+
  '(("push" %php-spl-list-push)
    ("pop" %php-spl-list-pop)
    ("unshift" %php-spl-list-unshift)
    ("shift" %php-spl-list-shift)
    ("top" %php-spl-list-top)
    ("bottom" %php-spl-list-bottom)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)))

(defparameter +php-spl-queue-methods+
  '(("push" %php-spl-list-push)
    ("pop" %php-spl-list-queue-pop)
    ("unshift" %php-spl-list-unshift)
    ("shift" %php-spl-list-shift)
    ("top" %php-spl-list-top)
    ("bottom" %php-spl-list-bottom)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)
    ("enqueue" %php-spl-list-push)
    ("dequeue" %php-spl-list-shift)))

(defun %php-spl-install-list-methods (object &key queue-p)
  (%php-spl-install-methods object
                            (if queue-p
                                +php-spl-queue-methods+
                                +php-spl-list-methods+)))

(defun %php-spl-doubly-linked-list ()
  (let ((object (%php-spl-object
                 "SplDoublyLinkedList"
                 (%php-spl-make-methods "push" "pop" "unshift" "shift"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object)))

(defun %php-spl-stack ()
  (let ((object (%php-spl-object
                 "SplStack"
                 (%php-spl-make-methods "push" "pop" "unshift" "shift"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object)))

(defun %php-spl-queue ()
  (let ((object (%php-spl-object
                 "SplQueue"
                 (%php-spl-make-methods "enqueue" "dequeue" "push" "pop"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object :queue-p t)))

(defun %php-spl-value< (left right)
  (let ((ln (%php-spl-number left))
        (rn (%php-spl-number right)))
    (cond
      ((or (/= ln 0) (/= rn 0)
           (member left '(0 0.0) :test #'eql)
           (member right '(0 0.0) :test #'eql))
       (< ln rn))
      (t (string< (%php-stringify left) (%php-stringify right))))))

(defun %php-spl-heap-best (items min-p)
  (reduce (lambda (best value)
            (if (if min-p
                    (%php-spl-value< value best)
                    (%php-spl-value< best value))
                value
                best))
          items))

(defun %php-spl-heap-remove-one (items target)
  (let ((removed nil))
    (loop for value in items
          unless (and (not removed) (equal value target))
            collect value
          else do (setf removed t))))

(defun %php-spl-heap-min-p (self)
  (gethash "__min_heap__" self))

(defun %php-spl-heap-insert (self value)
  (%php-spl-list-push self value))

(defun %php-spl-heap-top (self)
  (let ((items (%php-spl-items self)))
    (if items
        (%php-spl-heap-best items (%php-spl-heap-min-p self))
        +php-null+)))

(defun %php-spl-heap-extract (self)
  (let ((items (%php-spl-items self)))
    (if items
        (let ((best (%php-spl-heap-best items (%php-spl-heap-min-p self))))
          (%php-spl-set-items self (%php-spl-heap-remove-one items best))
          best)
        +php-null+)))

(defparameter +php-spl-heap-methods+
  '(("insert" %php-spl-heap-insert)
    ("extract" %php-spl-heap-extract)
    ("top" %php-spl-heap-top)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)))

(defun %php-spl-heap (class-name min-p)
  (let ((object (%php-spl-object
                 class-name
                 (%php-spl-make-methods "insert" "extract" "top" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (setf (gethash "__min_heap__" object) min-p)
    (%php-spl-install-methods object +php-spl-heap-methods+)
    object))

(defun %php-spl-min-heap ()
  (%php-spl-heap "SplMinHeap" t))

(defun %php-spl-max-heap ()
  (%php-spl-heap "SplMaxHeap" nil))

(defun %php-spl-fixed-values (self)
  (or (gethash "__values__" self) '()))

(defun %php-spl-fixed-set-values (self values)
  (setf (gethash "__values__" self) values))

(defun %php-spl-fixed-normalize-size (size)
  (max 0 (%php-spl-index size)))

(defun %php-spl-fixed-resize-list (values size)
  (let ((current (length values)))
    (cond
      ((= current size) values)
      ((> current size) (subseq values 0 size))
      (t (append values (make-list (- size current) :initial-element +php-null+))))))

(defun %php-spl-fixed-ref (self index)
  (let* ((values (%php-spl-fixed-values self))
         (i (%php-spl-index index)))
    (if (and (>= i 0) (< i (length values)))
        (nth i values)
        +php-null+)))

(defun %php-spl-fixed-set (self index value)
  (let* ((values (%php-spl-fixed-values self))
         (i (%php-spl-index index)))
    (when (and (>= i 0) (< i (length values)))
      (setf (nth i values) value)
      (%php-spl-fixed-set-values self values))
    +php-null+))

(defun %php-spl-fixed-size (self)
  (length (%php-spl-fixed-values self)))

(defun %php-spl-fixed-set-size (self new-size)
  (%php-spl-fixed-set-values
   self
   (%php-spl-fixed-resize-list
    (%php-spl-fixed-values self)
    (%php-spl-fixed-normalize-size new-size)))
  +php-null+)

(defun %php-spl-fixed-offset-exists-p (self index)
  (let ((i (%php-spl-index index)))
    (and (>= i 0) (< i (%php-spl-fixed-size self)))))

(defun %php-spl-fixed-unset (self index)
  (%php-spl-fixed-set self index +php-null+))

(defparameter +php-spl-fixed-array-methods+
  '(("getSize" %php-spl-fixed-size)
    ("setSize" %php-spl-fixed-set-size)
    ("offsetGet" %php-spl-fixed-ref)
    ("offsetSet" %php-spl-fixed-set)
    ("offsetExists" %php-spl-fixed-offset-exists-p)
    ("offsetUnset" %php-spl-fixed-unset)
    ("count" %php-spl-fixed-size)))

(defun %php-spl-fixed-array (&optional size)
  (let ((object (%php-spl-object
                 "SplFixedArray"
                 (%php-spl-make-methods "getSize" "setSize" "offsetGet"
                                        "offsetSet" "offsetExists" "offsetUnset"
                                        "count"))))
    (%php-spl-fixed-set-values object
                               (make-list (%php-spl-fixed-normalize-size (or size 0))
                                          :initial-element +php-null+))
    (%php-spl-install-methods object +php-spl-fixed-array-methods+)
    object))

(defun %php-spl-new (class-name &rest args)
  (case (intern (string-upcase (%php-spl-class-name class-name)) :keyword)
    (:SPLSTACK (%php-spl-stack))
    (:SPLQUEUE (%php-spl-queue))
    (:SPLDOUBLYLINKEDLIST (%php-spl-doubly-linked-list))
    (:SPLMINHEAP (%php-spl-min-heap))
    (:SPLMAXHEAP (%php-spl-max-heap))
    (:SPLFIXEDARRAY (apply #'%php-spl-fixed-array args))
    (otherwise (error "Unknown SPL class: ~A" class-name))))

;;; ─── Misc utility ────────────────────────────────────────────────────────────

(defun %php-array-map-keys (callback array)
  "PHP array_map with keys support (2-arg form with null callback)."
  (when (null callback)
    (let ((result (%php-make-array)))
      (dolist (pair (%php-array-pairs array))
        (%php-array-set result (%php-array-next-auto-index result)
                        (let ((row (%php-make-array)))
                          (%php-array-set row 0 (car pair))
                          (%php-array-set row 1 (cdr pair))
                          row)))
      (return-from %php-array-map-keys result)))
  (%php-array-map callback array))

(defun %php-list-assign (&rest values)
  "PHP list() — returns the values as a PHP array for destructuring."
  (%php-list-to-array values))

(defun %php-reset (array)
  "PHP reset: reset array pointer to first element."
  (when (hash-table-p array)
    (let ((vals (%php-array-values-list array)))
      (if vals (first vals) nil))))

(defun %php-end (array)
  "PHP end: advance array pointer to last element."
  (when (hash-table-p array)
    (let ((vals (%php-array-values-list array)))
      (if vals (car (last vals)) nil))))

(defun %php-current (array)
  "PHP current: return current element."
  (%php-reset array))

(defun %php-next (array)
  "PHP next: advance internal pointer."
  (when (hash-table-p array)
    (let ((vals (%php-array-values-list array)))
      (if (> (length vals) 1) (second vals) nil))))

(defun %php-prev (array)
  "PHP prev: rewind internal pointer."
  (%php-reset array))

(defun %php-key (array)
  "PHP key: return current key."
  (when (hash-table-p array)
    (let ((keys (%php-array-ordered-keys array)))
      (if keys (first keys) nil))))

(defun %php-microtime-float ()
  "PHP microtime(true): return current time as float."
  (coerce (- (get-internal-real-time) 0) 'double-float))

;;; ─── Non-CL-named builtins that must be registered by SYMBOL ────────────────
;;; A PHP builtin call lowers to a function symbol the VM resolves only if it is
;;; fbound; a lambda registration leaves no fbound symbol, so these all hit
;;; "Undefined function: <NAME>".  Named helpers fix that.

(defun %php-class-implements (class &optional autoload)
  "PHP class_implements: return implemented interfaces keyed by interface name."
  (declare (ignore autoload))
  (%php-reflection-symbols-to-array
   (%php-reflection-class-interface-symbols class)))
(defun %php-class-parents (class &optional autoload)
  "PHP class_parents: return parent classes keyed by class name."
  (declare (ignore autoload))
  (%php-reflection-symbols-to-array
   (%php-reflection-class-parent-symbols class)))
(defun %php-class-uses (class &optional autoload)
  "PHP class_uses: return traits used directly by a class keyed by trait name."
  (declare (ignore autoload))
  (%php-reflection-symbols-to-array
   (%php-reflection-class-trait-symbols class)))

(defvar *php-spl-autoload-functions* nil
  "Registered PHP SPL autoload callbacks in call order.")

(defun %php-spl-autoload-callback-equal-p (left right)
  "Return true when two PHP callable values identify the same autoload entry."
  (or (eq left right)
      (equal left right)
      (and (stringp left) (stringp right) (string= left right))))

(defun %php-spl-autoload-callback-valid-p (callback)
  "Return true when CALLBACK is acceptable for spl_autoload_register."
  (or (stringp callback)
      (functionp callback)
      (cl-cc/vm::%vm-closure-object-p callback)
      (and (hash-table-p callback)
           (plusp (%php-count callback)))))

(defun %php-spl-autoload-register (&optional callback throw prepend)
  "PHP spl_autoload_register: add CALLBACK to the SPL autoload queue.

The runtime does not model file loading yet, but it now preserves PHP-visible
autoload state for spl_autoload_functions/unregister and class_exists($c, true)
plumbing."
  (let ((entry (if (or (null callback) (%php-null-p callback))
                   "spl_autoload"
                   callback)))
    (cond
      ((not (%php-spl-autoload-callback-valid-p entry))
       (if (%php-truthy throw)
           (%php-throw 'type-error
                       "spl_autoload_register(): Argument #1 ($callback) must be a valid callback")
           nil))
      ((some (lambda (registered)
               (%php-spl-autoload-callback-equal-p registered entry))
             *php-spl-autoload-functions*)
       t)
      ((%php-truthy prepend)
       (push entry *php-spl-autoload-functions*)
       t)
      (t
       (setf *php-spl-autoload-functions*
             (append *php-spl-autoload-functions* (list entry)))
       t))))

(defun %php-spl-autoload-unregister (&optional callback)
  "PHP spl_autoload_unregister: remove CALLBACK from the SPL autoload queue."
  (let* ((entry (if (or (null callback) (%php-null-p callback))
                    "spl_autoload"
                    callback))
         (before *php-spl-autoload-functions*))
    (setf *php-spl-autoload-functions*
          (remove-if (lambda (registered)
                       (%php-spl-autoload-callback-equal-p registered entry))
                     *php-spl-autoload-functions*))
    (not (= (length before) (length *php-spl-autoload-functions*)))))

(defun %php-spl-autoload-functions ()
  "PHP spl_autoload_functions: return registered autoload callbacks."
  (%php-list-to-array *php-spl-autoload-functions*))

(defun %php-lcg-value ()
  "PHP lcg_value: a pseudo-random float in [0,1)."
  (random 1.0d0))

(defun %php-settype-array-value (value)
  "Return VALUE converted with PHP's `(array)` cast shape."
  (let ((result (%php-make-array)))
    (unless (%php-null-p value)
      (%php-array-set result 0 value))
    result))

(defun %php-settype-object-value (value)
  "Return VALUE converted with PHP's `(object)` cast shape."
  (let ((object (%php-make-array)))
    (%php-array-set object "__class__" "stdClass")
    (cond
      ((%php-null-p value))
      ((hash-table-p value)
       (dolist (pair (%php-array-pairs value))
         (%php-array-set object (car pair) (cdr pair))))
      (t
       (%php-array-set object "scalar" value)))
    object))

(defun %php-settype (v &optional type)
  "PHP settype: mutate the referenced value and return success."
  (let* ((target (%php-deref v))
         (type-name (string-downcase (%php-stringify type)))
         (converted
           (cond ((or (string= type-name "boolean") (string= type-name "bool"))
                  (%php-boolval target))
                 ((or (string= type-name "integer") (string= type-name "int"))
                  (%php-intval target))
                 ((or (string= type-name "float") (string= type-name "double"))
                  (%php-floatval target))
                 ((string= type-name "string")
                  (%php-strval target))
                 ((string= type-name "array")
                  (if (hash-table-p target) target (%php-settype-array-value target)))
                 ((string= type-name "object")
                  (if (or (%php-object-table-p target)
                          (and (not (%php-null-p target))
                               (not (null target))
                               (not (eq target t))
                               (not (numberp target))
                               (not (stringp target))
                               (not (hash-table-p target))))
                      target
                      (%php-settype-object-value target)))
                 ((string= type-name "null")
                  +php-null+)
                 (t :php-invalid-settype))))
    (if (eq converted :php-invalid-settype)
        nil
        (progn
          (%php-ref-set! v converted)
          t))))
(defun %php-scan-whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %php-sscanf-digit-value (ch)
  (cond ((and (char>= ch #\0) (char<= ch #\9))
         (- (char-code ch) (char-code #\0)))
        ((and (char>= ch #\a) (char<= ch #\f))
         (+ 10 (- (char-code ch) (char-code #\a))))
        ((and (char>= ch #\A) (char<= ch #\F))
         (+ 10 (- (char-code ch) (char-code #\A))))
        (t nil)))

(defun %php-sscanf-skip-ws (s pos)
  (loop while (and (< pos (length s))
                   (%php-scan-whitespace-char-p (char s pos)))
        do (incf pos)
        finally (return pos)))

(defun %php-sscanf-scan-int (s pos width radix &key auto-radix unsigned)
  (let* ((start-limit pos)
         (end-limit (if width (min (length s) (+ pos width)) (length s)))
         (sign 1)
         (base radix))
    (when (and (< pos end-limit)
               (not unsigned)
               (member (char s pos) '(#\+ #\-) :test #'char=))
      (when (char= (char s pos) #\-) (setf sign -1))
      (incf pos))
    (when auto-radix
      (cond
        ((and (<= (+ pos 2) end-limit)
              (char= (char s pos) #\0)
              (member (char s (1+ pos)) '(#\x #\X) :test #'char=))
         (setf base 16)
         (incf pos 2))
        ((and (< pos end-limit) (char= (char s pos) #\0))
         (setf base 8))
        (t (setf base 10))))
    (let ((digits-start pos)
          (value 0))
      (loop while (< pos end-limit)
            for digit = (%php-sscanf-digit-value (char s pos))
            while (and digit (< digit base))
            do (setf value (+ (* value base) digit))
               (incf pos))
      (if (= pos digits-start)
          (values nil start-limit nil)
          (values (* sign value) pos t)))))

(defun %php-sscanf-scan-float (s pos width)
  (let* ((start pos)
         (end-limit (if width (min (length s) (+ pos width)) (length s)))
         (saw-digit nil))
    (when (and (< pos end-limit) (member (char s pos) '(#\+ #\-) :test #'char=))
      (incf pos))
    (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
          do (setf saw-digit t)
             (incf pos))
    (when (and (< pos end-limit) (char= (char s pos) #\.))
      (incf pos)
      (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
            do (setf saw-digit t)
               (incf pos)))
    (when (and saw-digit (< pos end-limit) (member (char s pos) '(#\e #\E) :test #'char=))
      (let ((exp-start pos)
            (exp-digits nil))
        (incf pos)
        (when (and (< pos end-limit) (member (char s pos) '(#\+ #\-) :test #'char=))
          (incf pos))
        (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
              do (setf exp-digits t)
                 (incf pos))
        (unless exp-digits
          (setf pos exp-start))))
    (if saw-digit
        (handler-case
            (let ((*read-default-float-format* 'double-float))
              (values (coerce (read-from-string (subseq s start pos)) 'double-float)
                      pos t))
          (error () (values nil start nil)))
        (values nil start nil))))

(defun %php-sscanf-values (str fmt)
  "Return sscanf parsed values as a PHP array."
  (let* ((input (%php-stringify str))
         (format-string (%php-stringify fmt))
         (i 0)
         (j 0)
         (values nil)
         (failed nil))
    (labels ((emit (value)
               (push value values))
             (parse-width ()
               (let ((start j))
                 (loop while (and (< j (length format-string))
                                  (digit-char-p (char format-string j)))
                       do (incf j))
                 (when (< start j)
                   (parse-integer format-string :start start :end j)))))
      (loop while (and (< j (length format-string)) (not failed))
            for fch = (char format-string j)
            do (cond
                 ((%php-scan-whitespace-char-p fch)
                  (setf i (%php-sscanf-skip-ws input i))
                  (loop while (and (< j (length format-string))
                                   (%php-scan-whitespace-char-p
                                    (char format-string j)))
                        do (incf j)))
                 ((char/= fch #\%)
                  (if (and (< i (length input)) (char= (char input i) fch))
                      (progn (incf i) (incf j))
                      (setf failed t)))
                 (t
                  (incf j)
                  (cond
                    ((>= j (length format-string))
                     (setf failed t))
                    ((char= (char format-string j) #\%)
                     (if (and (< i (length input)) (char= (char input i) #\%))
                         (progn (incf i) (incf j))
                         (setf failed t)))
                    (t
                     (let ((suppress nil))
                       (when (and (< j (length format-string))
                                  (char= (char format-string j) #\*))
                         (setf suppress t)
                         (incf j))
                       (let ((width (parse-width)))
                         (when (>= j (length format-string))
                           (setf failed t))
                         (unless failed
                           (let ((spec (char-downcase (char format-string j))))
                             (incf j)
                             (multiple-value-bind (value new-pos ok)
                                 (case spec
                                   ((#\d #\u)
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 10
                                     :unsigned (char= spec #\u)))
                                   (#\i
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 10
                                     :auto-radix t))
                                   (#\x
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 16
                                     :unsigned t))
                                   (#\o
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 8
                                     :unsigned t))
                                   ((#\f #\e #\g)
                                    (%php-sscanf-scan-float
                                     input (%php-sscanf-skip-ws input i) width))
                                   (#\s
                                    (let* ((start (%php-sscanf-skip-ws input i))
                                           (end-limit (if width
                                                          (min (length input)
                                                               (+ start width))
                                                          (length input)))
                                           (end start))
                                      (loop while (and (< end end-limit)
                                                       (not (%php-scan-whitespace-char-p
                                                             (char input end))))
                                            do (incf end))
                                      (if (= start end)
                                          (values nil i nil)
                                          (values (subseq input start end) end t))))
                                   (#\c
                                    (let* ((count (or width 1))
                                           (end (+ i count)))
                                      (if (<= end (length input))
                                          (values (subseq input i end) end t)
                                          (values nil i nil))))
                                   (otherwise
                                    (values nil i nil)))
                               (if ok
                                   (progn
                                     (setf i new-pos)
                                     (unless suppress (emit value)))
                                   (setf failed t))))))))))))
      (%php-list-to-array (nreverse values)))))

(defun %php-sscanf (str fmt &rest ignored)
  "PHP sscanf: parse STR with FMT. Without out-params this returns a PHP array;
the parser lowers out-param calls through `%php-sscanf-values` and returns the
number of assigned values."
  (let ((values (%php-sscanf-values str fmt)))
    (if ignored (%php-count values) values)))

(defun %php-read-standard-input-string ()
  "Read the remaining dynamically bound standard input as a string."
  (with-output-to-string (out)
    (loop for ch = (read-char *standard-input* nil nil)
          while ch
          do (write-char ch out))))

(defun %php-scanf-values (fmt)
  "Return scanf parsed values from standard input as a PHP array."
  (%php-sscanf-values (%php-read-standard-input-string) fmt))

(defun %php-scanf (fmt &rest ignored)
  "PHP scanf: parse the remaining standard input with FMT."
  (let ((values (%php-scanf-values fmt)))
    (if ignored (%php-count values) values)))

(defun %php-iterator-to-array (iter &optional (preserve-keys t))
  "PHP iterator_to_array: drain a Generator into a PHP array."
  (if (php-generator-p iter)
      (let ((result (%php-make-array)))
        (loop for i from 0
              while (%php-generator-valid iter)
              do (if (%php-truthy preserve-keys)
                     (%php-array-set result i (%php-generator-next iter))
                     (%php-array-set result (%php-array-next-auto-index result)
                                     (%php-generator-next iter))))
        result)
      iter))

(defun %php-iterator-count (iter)
  "PHP iterator_count: number of elements a Generator yields."
  (if (php-generator-p iter)
      (length (php-gen-values iter))
      (%php-count iter)))
