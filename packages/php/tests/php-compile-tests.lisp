(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest php-parser-cli-compile-path-for-php-files
  "Characterization: native compile path should auto-detect .php files and compile PHP source end-to-end."
  (let* ((tmp-dir (uiop:temporary-directory))
         (input (merge-pathnames "cl-cc-php-compile-gap.php" tmp-dir))
         (output (merge-pathnames "cl-cc-php-compile-gap" tmp-dir)))
    (with-open-file (stream input :direction :output :if-exists :supersede)
      (write-line "<?php echo match($x) { 1 => 'one', default => 'other' };" stream))
    (let ((result (cl-cc::compile-file-to-native input :output-file output)))
      (assert-equal output result)
      (assert-true (probe-file output)))))
