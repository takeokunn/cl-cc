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
(deftest php-grammar-stmt-for-loop-has-five-children
  "for loop CST node has kind :for with 5 children; the fifth is the :body node."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php for ($i = 0; $i < 10; $i = $i + 1) { echo $i; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :for (cl-cc:cst-node-kind node))
      (assert-= 5 (length (cl-cc:cst-interior-children node)))
      (let ((body-node (fifth (cl-cc:cst-interior-children node))))
        (assert-eq :body (cl-cc:cst-node-kind body-node))))))

(deftest php-grammar-stmt-foreach-key-value-has-four-children
  "foreach $k => $v CST node has kind :foreach with 4 children."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php foreach ($arr as $k => $v) { echo $k; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :foreach (cl-cc:cst-node-kind node))
      (assert-= 4 (length (cl-cc:cst-interior-children node))))))

;;; P3a+b: function return type and param defaults
(deftest php-grammar-stmt-function-with-return-type
  "function with : int return type annotation produces a :function-def CST node."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function add($a, $b): int { return $a + $b; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :function-def (cl-cc:cst-node-kind (first forms)))))

(deftest php-grammar-stmt-function-with-default-param
  "function with default param value produces a :function-def CST node."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php function greet($name = \"world\") { return $name; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :function-def (cl-cc:cst-node-kind (first forms)))))

;;; P3c: class implements — single and multiple
(deftest php-grammar-stmt-class-single-implements
  "class with a single implements interface produces a :class-def CST node."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements Iface { public function get($x) { return $x; } }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :class-def (cl-cc:cst-node-kind (first forms)))))

(deftest php-grammar-stmt-class-multiple-implements
  "class with multiple comma-separated interfaces produces a :class-def CST node."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php class Box implements IfaceA, IfaceB { public $x; }")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (assert-eq :class-def (cl-cc:cst-node-kind (first forms)))))

;;; P4: structural assertions on simple statements
(deftest php-grammar-stmt-echo-has-two-children
  "echo 42; produces a :echo CST node with 2 children (keyword + expression)."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php echo 42;")
    (assert-equal nil diagnostics)
    (assert-= 1 (length forms))
    (let ((node (first forms)))
      (assert-eq :echo (cl-cc:cst-node-kind node))
      (assert-= 2 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-return-with-value-has-two-children
  "return 42; produces a :return CST node with 2 children (keyword + expression)."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return 42;")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :return (cl-cc:cst-node-kind node))
      (assert-= 2 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-bare-return-has-one-child
  "return; (without value) produces a :return CST node with 1 child."
  (destructuring-bind (forms diagnostics)
      (%php-cst-parse-one "<?php return;")
    (assert-equal nil diagnostics)
    (let ((node (first forms)))
      (assert-eq :return (cl-cc:cst-node-kind node))
      (assert-= 1 (length (cl-cc:cst-interior-children node))))))

(deftest php-grammar-stmt-if-only-has-then-no-else
  "if-only CST node has :then interior child but no :else child."
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

(deftest php-grammar-stmt-if-else-has-both-then-and-else
  "if-else CST node has both :then and :else interior children."
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
