(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defun %php-cst-parse-one (source)
  "Parse SOURCE into CST forms and diagnostics, returning both." 
  (multiple-value-list (cl-cc/parse::parse-php-source-to-cst source)))

(deftest-each php-grammar-stmt-basic-dispatch
  "The CST statement dispatcher parses representative keyword-led statements without diagnostics."
  :cases (("echo" "<?php echo 1;")
          ("return" "<?php return 1;")
          ("if-else" "<?php if ($x) { echo 1; } else { echo 2; }")
          ("while" "<?php while ($x) { echo 1; }")
          ("foreach" "<?php foreach ($items as $item) { echo $item; }")
          ("function" "<?php function greet($name) { return $name; }")
          ("class" "<?php class Box extends Base { public function get($x) { return $x; } }")
          ("try-catch" "<?php try { echo 1; } catch (Ex $e) { echo 2; }")
          ("break" "<?php break;")
          ("continue" "<?php continue;"))
  (source)
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one source)
    (assert-true (consp forms))
    (assert-equal nil diagnostics)))

(deftest php-grammar-stmt-expression-fallback
  "Non-keyword statements fall back to expression parsing and still produce a CST node."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php $x + 1;")
    (assert-= 1 (length forms))
    (assert-true (first forms))
    (assert-equal nil diagnostics)))

;;; P1: for loop parser — zero tests existed before
(deftest php-grammar-stmt-for-loop
  "The for loop parser produces a :for CST node with init, cond, incr, and body."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php for ($i = 0; $i < 10; $i = $i + 1) { echo $i; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :for (cl-cc:cst-node-kind node))
      ;; children: kw init cond incr (body ...)
      (assert-= 5 (length (cl-cc:cst-interior-children node)))
      (let ((body-node (fifth (cl-cc:cst-interior-children node))))
        (assert-eq :body (cl-cc:cst-node-kind body-node))))))

;;; P2: foreach $k => $v two-variable form
(deftest php-grammar-stmt-foreach-key-value
  "foreach ($arr as $k => $v) parses the two-variable arrow form."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php foreach ($arr as $k => $v) { echo $k; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :foreach (cl-cc:cst-node-kind node))
      ;; children: kw arr var (body ...)
      (assert-= 4 (length (cl-cc:cst-interior-children node))))))

;;; P3a: return type annotation — %php-cst-skip-return-type
(deftest php-grammar-stmt-function-return-type
  "function with ': int' return type parses without error."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function add($a, $b): int { return $a + $b; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :function-def (cl-cc:cst-node-kind (first forms)))))

;;; P3b: parameter default values — the (= default) branch in php-cst-parse-param-list
(deftest php-grammar-stmt-function-param-default
  "function parameters with default values parse correctly."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function greet($name = \"world\") { return $name; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :function-def (cl-cc:cst-node-kind (first forms)))))

;;; P3c: class implements — the implements consumption loop
(deftest php-grammar-stmt-class-implements
  "class with implements interface parses without error."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements Iface { public function get($x) { return $x; } }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :class-def (cl-cc:cst-node-kind (first forms)))))

(deftest php-grammar-stmt-class-implements-multiple
  "class with multiple implements (comma-separated) parses without error."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements IfaceA, IfaceB { public $x; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :class-def (cl-cc:cst-node-kind (first forms)))))

;;; P4: structural assertions on existing dispatch tests
(deftest php-grammar-stmt-echo-structure
  "echo statement produces correct CST structure."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php echo 42;")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :echo (cl-cc:cst-node-kind node))
      (assert-= 2 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-return-value-structure
  "return with expression has two children (keyword + expr)."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return 42;")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :return (cl-cc:cst-node-kind node))
      (assert-= 2 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-return-empty-structure
  "bare return has one child (keyword only)."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return;")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :return (cl-cc:cst-node-kind node))
      (assert-= 1 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-if-then-only
  "if without else has :then but no :else child."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php if ($x) { echo 1; }")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :if (cl-cc:cst-node-kind node))
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (assert-true (member :then kinds))
        (assert-false (member :else kinds))))))

(deftest php-grammar-stmt-if-then-else
  "if-else has both :then and :else children."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php if ($x) { echo 1; } else { echo 2; }")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :if (cl-cc:cst-node-kind node))
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (assert-true (member :then kinds))
        (assert-true (member :else kinds))))))

(deftest php-grammar-stmt-try-catch-structure
  "try-catch has :try-body and at least one :catch child."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php try { echo 1; } catch (Ex $e) { echo 2; }")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :try-catch (cl-cc:cst-node-kind node))
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (assert-true (member :try-body kinds))
        (assert-true (member :catch kinds))))))
