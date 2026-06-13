;;;; packages/javascript/tests/js-runtime-date-json-tests.lisp
;;;;
;;;; Temporal helper functions, Date.prototype, JSON stringify, JSON parse.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Temporal helper functions ───────────────────────────────────────────────

(deftest-each js-rt-temporal-pad
  "%temporal-pad zero-pads integers to the specified width."
  :cases (("year-4"   2025 4 "2025")
          ("month-2"  3    2 "03")
          ("day-2"    15   2 "15")
          ("narrow"   9    1 "9"))
  (n width expected)
  (assert-string= expected (cl-cc/javascript::%temporal-pad n width)))

(deftest-each js-rt-temporal-3way-compare
  "%temporal-3way-compare returns -1/0/1 for ordered numeric comparison."
  :cases (("less"    1 2 -1.0d0)
          ("equal"   5 5  0.0d0)
          ("greater" 9 3  1.0d0))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%temporal-3way-compare a b)))

(deftest-each js-rt-temporal-parse-iso-fields
  "%temporal-parse-iso-fields decomposes an ISO-8601 datetime string."
  :cases (("full"      "2025-06-13T14:30:00" 2025 6  13 14 30 0)
          ("date-only" "2025-01-01"           2025 1   1  0  0 0))
  (s exp-y exp-mo exp-d exp-h exp-mi exp-s)
  (multiple-value-bind (y mo d h mi s) (cl-cc/javascript::%temporal-parse-iso-fields s)
    (assert-= exp-y  y)
    (assert-= exp-mo mo)
    (assert-= exp-d  d)
    (assert-= exp-h  h)
    (assert-= exp-mi mi)
    (assert-= exp-s  s)))

(deftest-each js-rt-temporal-duration-to-seconds
  "%temporal-duration-to-seconds converts duration hash-tables to total seconds."
  :cases (("one-hour"   "hours"   1 3600)
          ("one-minute" "minutes" 1 60)
          ("one-second" "seconds" 1 1)
          ("one-day"    "days"    1 86400))
  (unit n expected)
  (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (assert-= expected (cl-cc/javascript::%temporal-duration-to-seconds dur))))

(deftest js-rt-temporal-parse-time-fields
  "%temporal-parse-time-fields decomposes an HH:MM:SS string."
  (multiple-value-bind (h m s) (cl-cc/javascript::%temporal-parse-time-fields "14:30:05")
    (assert-= 14 h)
    (assert-= 30 m)
    (assert-=  5 s)))

;;; ─── Date.prototype ──────────────────────────────────────────────────────────

(deftest js-rt-date-now
  "Date.now() returns a positive integer (milliseconds since Unix epoch)."
  (let ((t1 (cl-cc/javascript::%js-date-now))
        (t2 (cl-cc/javascript::%js-date-now)))
    (assert-true (integerp t1))
    (assert-true (>= t2 t1))))

(deftest js-rt-date-make-date-no-args
  "%js-make-date with no args returns a js-date struct."
  (let ((d (cl-cc/javascript::%js-make-date)))
    (assert-true (cl-cc/javascript::js-date-p d))
    (assert-true (integerp (cl-cc/javascript::js-date-ms d)))))

(deftest js-rt-date-make-date-from-ms
  "%js-make-date from a millisecond value stores the ms directly."
  (let ((d (cl-cc/javascript::%js-make-date 1000000.0d0)))
    (assert-= 1000000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-make-date-copy
  "%js-make-date from another Date copies the ms."
  (let* ((orig (cl-cc/javascript::%js-make-date 42000.0d0))
         (copy (cl-cc/javascript::%js-make-date orig)))
    (assert-= 42000 (cl-cc/javascript::js-date-ms copy))))

(deftest js-rt-date-parse-string-date-only
  "%js-date-parse-string parses YYYY-MM-DD to ms."
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01")))
    (assert-= 0 ms)))

(deftest js-rt-date-parse-string-datetime
  "%js-date-parse-string parses YYYY-MM-DDTHH:MM:SS."
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01T01:00:00")))
    (assert-= 3600000 ms)))

(deftest js-rt-date-parse-string-error
  "%js-date-parse-string falls back to now on invalid input."
  (let ((result (cl-cc/javascript::%js-date-parse-string "not-a-date")))
    (assert-true (integerp result))))

;;; 97445000 ms = 1970-01-02T03:04:05.000Z
(deftest js-rt-date-getters
  "Date.prototype getters return correct decomposed fields for a fixed epoch."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-= 1970 (cl-cc/javascript::%js-date-get-full-year d))
    (assert-= 1970 (cl-cc/javascript::%js-date-get-utc-full-year d))
    (assert-= 0    (cl-cc/javascript::%js-date-get-month d))      ; January = 0
    (assert-= 2    (cl-cc/javascript::%js-date-get-date d))
    (assert-= 3    (cl-cc/javascript::%js-date-get-hours d))
    (assert-= 4    (cl-cc/javascript::%js-date-get-minutes d))
    (assert-= 5    (cl-cc/javascript::%js-date-get-seconds d))
    (assert-= 0    (cl-cc/javascript::%js-date-get-milliseconds d))))

(deftest js-rt-date-get-time
  "Date.prototype.getTime returns ms as double-float."
  (let ((d (cl-cc/javascript::%js-make-date 12345)))
    (assert-= 12345.0d0 (cl-cc/javascript::%js-date-get-time d))))

(deftest js-rt-date-to-iso-string
  "toISOString formats as YYYY-MM-DDTHH:MM:SS.mmmZ."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970-01-02T03:04:05.000Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-to-iso-string-with-ms
  "toISOString includes sub-second milliseconds."
  (let ((d (cl-cc/javascript::%js-make-date 97445123)))
    (assert-string= "1970-01-02T03:04:05.123Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-to-local-date-string
  "toLocaleDateString returns YYYY/MM/DD."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970/01/02" (cl-cc/javascript::%js-date-to-local-date-string d))))

(deftest js-rt-date-to-time-string
  "toTimeString returns HH:MM:SS GMT+0000 (...)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "03:04:05 GMT+0000 (Coordinated Universal Time)"
                    (cl-cc/javascript::%js-date-to-time-string d))))

(deftest js-rt-date-set-time
  "setTime updates ms and returns the new value."
  (let ((d (cl-cc/javascript::%js-make-date 0)))
    (cl-cc/javascript::%js-date-set-time d 5000)
    (assert-= 5000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-set-full-year-preserves-time
  "setFullYear preserves the existing time components (was bug: zeroed them)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; 1970-01-02T03:04:05Z
    (cl-cc/javascript::%js-date-set-full-year d 2024.0d0)
    (assert-= 3 (cl-cc/javascript::%js-date-get-hours d))
    (assert-= 4 (cl-cc/javascript::%js-date-get-minutes d))
    (assert-= 5 (cl-cc/javascript::%js-date-get-seconds d))))

(deftest js-rt-date-set-month
  "setMonth changes the month (JS 0-based)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; January
    (cl-cc/javascript::%js-date-set-month d 5.0d0)       ; June (0-based)
    (assert-= 5 (cl-cc/javascript::%js-date-get-month d))))

(deftest js-rt-date-set-date
  "setDate changes the day of month."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; day 2
    (cl-cc/javascript::%js-date-set-date d 15.0d0)
    (assert-= 15 (cl-cc/javascript::%js-date-get-date d))))

(deftest js-rt-date-rebuild-preserves-ms
  "%js-date-rebuild preserves sub-second milliseconds."
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))  ; .999 ms
    (cl-cc/javascript::%js-date-rebuild d :sec 10)
    (assert-= 999 (cl-cc/javascript::%js-date-get-milliseconds d))))

;;; ─── JSON stringify ──────────────────────────────────────────────────────────

(deftest-each js-rt-json-stringify-primitives
  "JSON.stringify handles JS primitive values."
  :cases (("null"        cl-cc/javascript::+js-null+       "null")
          ("undefined"   cl-cc/javascript::+js-undefined+  "null")
          ("true"        t                                  "true")
          ("false"       nil                                "false")
          ("integer"     42.0d0                            "42")
          ("float"       1.5d0                             "1.5")
          ("string"      "hello"                           "\"hello\"")
          ("nan"         cl-cc/javascript::*js-nan-float*  "null"))
  (val expected)
  (assert-string= expected (cl-cc/javascript::%js-json-stringify val)))

(deftest js-rt-json-stringify-string-escapes
  "JSON.stringify escapes special characters in strings."
  (assert-string= "\"line1\\nline2\"" (cl-cc/javascript::%js-json-stringify "line1
line2"))
  (assert-string= "\"a\\tb\"" (cl-cc/javascript::%js-json-stringify "a	b"))
  (assert-string= "\"say \\\"hi\\\"\"" (cl-cc/javascript::%js-json-stringify "say \"hi\"")))

(deftest js-rt-json-stringify-array
  "JSON.stringify serializes JS arrays."
  (let ((arr (cl-cc/javascript::%js-make-array 1.0d0 2.0d0 3.0d0)))
    (assert-string= "[1,2,3]" (cl-cc/javascript::%js-json-stringify arr))))

(deftest js-rt-json-stringify-object
  "JSON.stringify serializes JS objects."
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 1.0d0 "y" 2.0d0))
         (result (cl-cc/javascript::%js-json-stringify obj)))
    (assert-true (cl-cc/javascript::%js-string-includes result "\"x\":1"))
    (assert-true (cl-cc/javascript::%js-string-includes result "\"y\":2"))))

(deftest js-rt-json-stringify-nested
  "JSON.stringify handles nested objects and arrays."
  (let* ((inner (cl-cc/javascript::%js-make-object "a" 1.0d0))
         (arr   (cl-cc/javascript::%js-make-array inner))
         (result (cl-cc/javascript::%js-json-stringify arr)))
    (assert-true (cl-cc/javascript::%js-string-includes result "{"))
    (assert-true (cl-cc/javascript::%js-string-includes result "\"a\":1"))))

;;; ─── JSON parse ──────────────────────────────────────────────────────────────

(deftest-each js-rt-json-parse-non-undefined
  "JSON.parse returns non-undefined for valid JSON literals."
  :cases (("null-lit"  "null")
          ("true-lit"  "true")
          ("false-lit" "false")
          ("number-42" "42")
          ("str-hello" "\"hello\""))
  (input)
  (let ((result (cl-cc/javascript::%js-json-parse input)))
    (assert-true (not (eq result cl-cc/javascript::+js-undefined+)))))

(deftest js-rt-json-parse-null
  "JSON.parse(\"null\") returns the JS null sentinel."
  (assert-true (eq cl-cc/javascript::+js-null+ (cl-cc/javascript::%js-json-parse "null"))))

(deftest js-rt-json-parse-booleans
  "JSON.parse handles true and false."
  (assert-true (eq t   (cl-cc/javascript::%js-json-parse "true")))
  (assert-true (eq nil (cl-cc/javascript::%js-json-parse "false"))))

(deftest-each js-rt-json-parse-number
  "JSON.parse converts numeric strings to double-float."
  :cases (("integer"   "42"   42.0d0)
          ("float"     "3.14" 3.14d0)
          ("negative"  "-1"   -1.0d0))
  (str expected)
  (assert-= expected (cl-cc/javascript::%js-json-parse str)))

(deftest js-rt-json-parse-string
  "JSON.parse converts quoted strings."
  (assert-string= "hello" (cl-cc/javascript::%js-json-parse "\"hello\""))
  (assert-string= "a
b" (cl-cc/javascript::%js-json-parse "\"a\\nb\"")))

(deftest js-rt-json-parse-array
  "JSON.parse builds an adjustable vector for arrays."
  (let ((arr (cl-cc/javascript::%js-json-parse "[1,2,3]")))
    (assert-true (cl-cc/javascript::%js-vec-p arr))
    (assert-= 3 (length arr))
    (assert-= 1.0d0 (aref arr 0))))

(deftest js-rt-json-parse-object
  "JSON.parse builds a hash-table for objects."
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"x\":1,\"y\":2}")))
    (assert-true (cl-cc/javascript::%js-ht-p obj))
    (assert-= 1.0d0 (gethash "x" obj))
    (assert-= 2.0d0 (gethash "y" obj))))

(deftest js-rt-json-parse-nested
  "JSON.parse handles nested structures."
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"arr\":[1,2]}")))
    (let ((arr (gethash "arr" obj)))
      (assert-true (cl-cc/javascript::%js-vec-p arr))
      (assert-= 2 (length arr)))))

(deftest-each js-rt-json-parse-whitespace
  "JSON.parse skips leading/trailing whitespace."
  :cases (("number"  "  42  "    42.0d0)
          ("string"  "  \"x\"  "  "x"))
  (input expected)
  (let ((result (cl-cc/javascript::%js-json-parse input)))
    (if (stringp expected)
        (assert-string= expected result)
        (assert-= expected result))))

(deftest js-rt-json-parse-invalid
  "JSON.parse returns +js-undefined+ on invalid input."
  (assert-true (eq cl-cc/javascript::+js-undefined+ (cl-cc/javascript::%js-json-parse "NOT_JSON"))))

(deftest js-rt-json-roundtrip
  "stringify then parse round-trips a JS object."
  (let* ((orig     (cl-cc/javascript::%js-make-object "name" "Alice" "age" 30.0d0))
         (json     (cl-cc/javascript::%js-json-stringify orig))
         (reparsed (cl-cc/javascript::%js-json-parse json)))
    (assert-string= "Alice" (gethash "name" reparsed))
    (assert-= 30.0d0 (gethash "age" reparsed))))
