;;; vm-terminal.lisp — FR-1100: Terminal control
;;;
;;; ANSI terminal escape sequences, terminal sizing, raw mode, and readline.

(in-package :cl-cc/vm)

(defun vm-isatty (stream)
  "Return T if STREAM is connected to a terminal."
  (or (interactive-stream-p stream)
      (ignore-errors (sb-unix:unix-isatty (sb-sys:fd-stream-fd stream)))))

(defun vm-terminal-size (&optional (stream *standard-output*))
  "Return (values columns rows) for the terminal attached to STREAM.
   Uses TIOCGWINSZ ioctl or ANSI cursor position report as fallback."
  (declare (ignore stream))
  (ignore-errors
    (let* ((output (uiop:run-program '("stty" "size") :output :string :error-output nil))
           (rows (with-input-from-string (in output) (read in nil nil)))
           (cols (with-input-from-string (in output) (read in nil nil) (read in nil nil))))
      (when (and cols rows)
        (return-from vm-terminal-size (values cols rows)))))
  (values 80 24))

(defun vm-ansi-color (stream color)
  "Emit ANSI escape sequence for COLOR on STREAM.
   Supported: :black, :red, :green, :yellow, :blue, :magenta, :cyan, :white
   and their bright variants :bright-red etc."
  (let ((code (ecase color
                (:black 30) (:red 31) (:green 32) (:yellow 33)
                (:blue 34) (:magenta 35) (:cyan 36) (:white 37)
                (:bright-black 90) (:bright-red 91) (:bright-green 92)
                (:bright-yellow 93) (:bright-blue 94) (:bright-magenta 95)
                (:bright-cyan 96) (:bright-white 97))))
    (format stream "~C[~Dm" #\Escape code)))

(defun vm-ansi-reset (stream)
  "Reset all ANSI attributes on STREAM."
  (format stream "~C[0m" #\Escape))

(defun vm-with-raw-terminal (thunk)
  "Execute THUNK with terminal in raw mode (no line buffering, no echo).
   Restores original terminal settings on exit."
  (unwind-protect
       (progn
         (ignore-errors (uiop:run-program '("stty" "raw" "-echo") :input nil :output nil :error-output nil))
         (funcall thunk))
    (ignore-errors (uiop:run-program '("stty" "sane") :input nil :output nil :error-output nil))))

(defun vm-read-key (&optional (stream *standard-input*))
  "Read a single keypress from STREAM without waiting for newline.
   Returns the character (or keyword for special keys)."
  (vm-with-raw-terminal (lambda () (read-char stream nil +eof-value+))))

(defun vm-readline (prompt &key (history nil))
  "Read a line with PROMPT, supporting basic editing and HISTORY.
   Delegates to host CL readline or provides line-editor stub."
  (declare (ignore history))
  (format *query-io* "~A" prompt)
  (force-output *query-io*)
  (read-line *query-io* nil nil))

(export '(vm-isatty
          vm-terminal-size
          vm-ansi-color
          vm-ansi-reset
          vm-with-raw-terminal
          vm-read-key
          vm-readline))
