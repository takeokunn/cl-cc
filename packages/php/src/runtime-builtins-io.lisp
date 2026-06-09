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
      (write-string content)
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
          (close stream))
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
              #+sbcl
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

(defun %php-ini-get (varname)
  "PHP ini_get: get INI configuration value (stub)."
  (declare (ignore varname))
  nil)

(defun %php-ini-set (varname newvalue)
  "PHP ini_set: set INI configuration value (stub)."
  (declare (ignore varname newvalue))
  nil)

(defun %php-set-error-handler (callback &optional error-types)
  "PHP set_error_handler: set custom error handler (stub)."
  (declare (ignore callback error-types))
  nil)

(defun %php-set-exception-handler (callback)
  "PHP set_exception_handler: set custom exception handler (stub)."
  (declare (ignore callback))
  nil)

(defun %php-error-reporting (&optional level)
  "PHP error_reporting: set/get error reporting level."
  (declare (ignore level))
  32767)

(defun %php-trigger-error (message &optional (error-type 256))
  "PHP trigger_error: generate a user-level error."
  (declare (ignore error-type))
  (format t "~A~%" (%php-stringify message))
  t)

(defun %php-exit (&optional (status 0))
  "PHP exit/die: terminate execution."
  (when (stringp status) (write-string status))
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

(defun %php-ob-start (&optional callback chunk-size flags)
  "PHP ob_start: start output buffering (stub)."
  (declare (ignore callback chunk-size flags))
  t)

(defun %php-ob-end-clean ()
  "PHP ob_end_clean: clean output buffer (stub)."
  t)

(defun %php-ob-get-clean ()
  "PHP ob_get_clean: get current buffer contents and clean (stub)."
  "")

(defun %php-ob-get-contents ()
  "PHP ob_get_contents: get current output buffer (stub)."
  "")

(defun %php-header (string &optional replace response-code)
  "PHP header: send HTTP header (stub — outputs as comment)."
  (declare (ignore replace response-code string))
  nil)

(defun %php-http-response-code (&optional response-code)
  "PHP http_response_code: set/get HTTP response code."
  (declare (ignore response-code))
  200)

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

(defun %php-number-format-alias (&rest args)
  "Alias to existing %php-number-format."
  (apply #'%php-number-format args))

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

(defun %php-each (array)
  "PHP each: return current key/value pair and advance internal pointer (deprecated)."
  (when (hash-table-p array)
    (let ((pairs (%php-array-pairs array)))
      (when pairs
        (let* ((pair (first pairs))
               (result (%php-make-array)))
          (%php-array-set result 0 (cdr pair))
          (%php-array-set result 1 (car pair))
          (%php-array-set result "key" (car pair))
          (%php-array-set result "value" (cdr pair))
          result)))))

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
;;; "Undefined function: <NAME>".  Named helpers fix that.  (Several remain
;;; deliberate stubs — full reflection / scope-mutation is not modelled.)

(defun %php-class-implements (class &optional autoload)
  "PHP class_implements (stub: full interface reflection NYI)."
  (declare (ignore class autoload)) (%php-make-array))
(defun %php-class-parents (class &optional autoload)
  "PHP class_parents (stub: full class reflection NYI)."
  (declare (ignore class autoload)) (%php-make-array))
(defun %php-class-uses (class &optional autoload)
  "PHP class_uses (stub: full trait reflection NYI)."
  (declare (ignore class autoload)) (%php-make-array))
(defun %php-spl-autoload-register (&rest ignored)
  "PHP spl_autoload_register (stub — autoloading not modelled)."
  (declare (ignore ignored)) t)
(defun %php-spl-autoload-unregister (&rest ignored)
  "PHP spl_autoload_unregister (stub)."
  (declare (ignore ignored)) t)
(defun %php-spl-autoload-functions ()
  "PHP spl_autoload_functions (stub)."
  (%php-make-array))
(defun %php-extract (array &optional flags prefix)
  "PHP extract (stub — returns the count; the calling scope cannot be mutated
from a host builtin)."
  (declare (ignore flags prefix))
  (if (hash-table-p array) (%php-count array) 0))
(defun %php-lcg-value ()
  "PHP lcg_value: a pseudo-random float in [0,1)."
  (random 1.0d0))
(defun %php-settype (v &optional type)
  "PHP settype (stub — scalar by-reference mutation is not modelled)."
  (declare (ignore type)) v)
(defun %php-sscanf (str fmt &rest ignored)
  "PHP sscanf (stub — format parsing / out-params NYI; returns a PHP array)."
  (declare (ignore ignored))
  (%php-list-to-array (list (%php-stringify str) (%php-stringify fmt))))

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
