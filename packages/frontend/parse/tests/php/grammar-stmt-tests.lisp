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

;;; P1+P2: for and foreach parsers
(deftest php-grammar-stmt-loop-cases
  "for loop produces :for node with 5 children; foreach key-value form produces :foreach with 4 children."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php for ($i = 0; $i < 10; $i = $i + 1) { echo $i; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :for (cl-cc:cst-node-kind node))
      (assert-= 5 (length (cl-cc:cst-interior-children node)))
      (let ((body-node (fifth (cl-cc:cst-interior-children node))))
        (assert-eq :body (cl-cc:cst-node-kind body-node)))))
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php foreach ($arr as $k => $v) { echo $k; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :foreach (cl-cc:cst-node-kind node))
      (assert-= 4 (length (cl-cc:cst-interior-children node))))))

;;; P3a+b: function return type and param defaults
(deftest php-grammar-stmt-function-cases
  "function with return type annotation and with default param values both parse correctly."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function add($a, $b): int { return $a + $b; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :function-def (cl-cc:cst-node-kind (first forms))))
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function greet($name = \"world\") { return $name; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :function-def (cl-cc:cst-node-kind (first forms)))))

;;; P3c: class implements — single and multiple
(deftest php-grammar-stmt-class-implements-cases
  "class with single implements and with multiple comma-separated interfaces both parse correctly."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements Iface { public function get($x) { return $x; } }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :class-def (cl-cc:cst-node-kind (first forms))))
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements IfaceA, IfaceB { public $x; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :class-def (cl-cc:cst-node-kind (first forms)))))

;;; P4: structural assertions on simple statements
(deftest php-grammar-stmt-simple-structure-cases
  "echo has 2 children; return+expr has 2 children; bare return has 1 child."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php echo 42;")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :echo (cl-cc:cst-node-kind node))
      (assert-= 2 (length (cl-cc:cst-interior-children node)))))
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return 42;")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :return (cl-cc:cst-node-kind node))
      (assert-= 2 (length (cl-cc:cst-interior-children node)))))
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return;")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :return (cl-cc:cst-node-kind node))
      (assert-= 1 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-if-cases
  "if-then-only has :then but no :else; if-then-else has both :then and :else children."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php if ($x) { echo 1; }")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :if (cl-cc:cst-node-kind node))
      (let ((kinds (mapcar #'cl-cc:cst-node-kind
                           (remove-if-not #'cl-cc:cst-interior-p
                                          (cl-cc:cst-interior-children node)))))
        (assert-true (member :then kinds))
        (assert-false (member :else kinds)))))
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
