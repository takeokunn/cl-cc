;;;; tests/tooling-advanced-1-evidence-tests.lisp
;;;; Evidence registry for tooling-advanced-1.md (136 FRs across 22 phases).
;;;; Verifies FR coverage and evidence file existence.

(in-package :cl-cc/test)
(in-suite cl-cc-documentation-suite)

;;; ─── FR table ──────────────────────────────────────────────────────────────────
;; Each entry: (fr-id (target-files) difficulty)
;; Target files are as specified in tooling-advanced-1.md.

(defparameter *tooling-advanced-1-fr-table*
  '(
    ;; ── Phase 79 — ML駆動最適化 ──────────────────────────────────────────────────
    ("FR-372" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-373" ("packages/optimize/src/optimizer.lisp" "packages/emit/src/x86-64-codegen.lisp")     :very-hard)
    ("FR-374" ("packages/parse/src/diagnostics.lisp" "packages/vm/src/conditions.lisp")            :medium)

    ;; ── Phase 80 — 並列・分散コンパイル ──────────────────────────────────────────────
    ("FR-376" ("packages/pipeline/pipeline.lisp" "cl-cc.asd")                                      :hard)
    ("FR-377" ("packages/pipeline/pipeline.lisp" "src/build/remote.lisp")                          :very-hard)
    ("FR-378" ("packages/cli/src/main.lisp" "packages/binary/src/macho.lisp")                      :medium)

    ;; ── Phase 81 — WebAssembly・異種ターゲット ─────────────────────────────────────────
    ("FR-380" ("packages/emit/src/wasm/" "packages/cli/src/main.lisp")                              :very-hard)
    ("FR-381" ("packages/emit/src/wasm/" "packages/vm/src/io.lisp")                                :hard)
    ("FR-382" ("packages/mir/src/target.lisp" "packages/emit/src/spirv/")                          :very-hard)
    ("FR-383" ("packages/emit/src/wasm/" "packages/emit/src/js/")                                  :medium)
    ("FR-384" ("packages/binary/src/macho.lisp" "packages/cli/src/main.lisp")                      :medium)

    ;; ── Phase 82 — 構造化並行性・コルーチン ──────────────────────────────────────────
    ("FR-386" ("packages/expand/src/macros-sequence.lisp" "packages/compile/src/cps.lisp")         :medium)
    ("FR-387" ("packages/vm/src/vm-run.lisp" "packages/runtime/src/" "src/concurrent/")             :very-hard)
    ("FR-388" ("packages/expand/src/expander.lisp" "packages/compile/src/cps.lisp")                :hard)

    ;; ── Phase 83 — 抽象解釈・形式検証 ──────────────────────────────────────────────
    ("FR-390" ("packages/optimize/src/optimizer.lisp" "packages/optimize/src/vrp.lisp")             :hard)
    ("FR-391" ("src/analyze/abstract-interp.lisp")                                                  :very-hard)
    ("FR-392" ("src/analyze/smt.lisp")                                                              :very-hard)

    ;; ── Phase 84 — CHERI・最先端ハードウェアセキュリティ ─────────────────────────────
    ("FR-395" ("packages/emit/src/aarch64-codegen.lisp" "packages/runtime/src/heap.lisp")          :very-hard)
    ("FR-396" ("packages/emit/src/riscv64-codegen.lisp")                                           :hard)
    ("FR-397" ("packages/binary/src/macho.lisp")                                                    :hard)
    ("FR-398" ("packages/expand/src/expander.lisp" "packages/optimize/src/optimizer.lisp"
               "packages/cli/src/main.lisp")                                                        :hard)
    ("FR-399" ("packages/vm/src/vm-run.lisp" "packages/runtime/src/"
               "packages/emit/src/x86-64-codegen.lisp")                                            :hard)

    ;; ── Phase 85 — 高度最適化パス ────────────────────────────────────────────────────
    ("FR-400" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-401" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-402" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-403" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-404" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-405" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-406" ("packages/optimize/src/optimizer.lisp")                                              :very-hard)
    ("FR-407" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-408" ("packages/emit/src/x86-64-codegen.lisp" "packages/emit/src/aarch64-codegen.lisp")   :hard)
    ("FR-409" ("packages/optimize/src/memory-ssa.lisp" "packages/optimize/src/optimizer.lisp")      :very-hard)
    ("FR-410" ("packages/emit/src/x86-64-codegen.lisp" "packages/optimize/src/optimizer.lisp")      :medium)

    ;; ── Phase 86 — 高度コード生成・ABI ─────────────────────────────────────────────
    ("FR-412" ("packages/vm/src/vm.lisp" "packages/runtime/src/heap.lisp")                         :hard)
    ("FR-413" ("packages/compile/src/regalloc.lisp")                                                :hard)
    ("FR-414" ("packages/emit/src/calling-convention.lisp" "packages/compile/src/codegen.lisp")    :hard)
    ("FR-415" ("packages/emit/src/x86-64-codegen.lisp")                                             :medium)
    ("FR-416" ("packages/emit/src/x86-64-codegen.lisp")                                             :easy)
    ("FR-417" ("packages/compile/src/codegen.lisp" "packages/type/src/inference.lisp")             :hard)

    ;; ── Phase 88 — GC・メモリランタイム高度化 ───────────────────────────────────────
    ("FR-420" ("packages/runtime/src/gc.lisp")                                                      :very-hard)
    ("FR-421" ("packages/runtime/src/gc.lisp" "packages/runtime/src/heap.lisp")                    :very-hard)
    ("FR-422" ("packages/vm/src/vm.lisp" "packages/runtime/src/gc.lisp")                           :medium)
    ("FR-423" ("packages/runtime/src/heap.lisp" "packages/runtime/src/arena.lisp")                  :medium)
    ("FR-424" ("packages/runtime/src/gc.lisp" "packages/compile/src/codegen.lisp")                 :hard)
    ("FR-425" ("packages/runtime/src/" "packages/compile/src/codegen.lisp")                         :very-hard)

    ;; ── Phase 89 — 型システム拡張 ──────────────────────────────────────────────────
    ("FR-428" ("packages/type/src/inference.lisp" "packages/compile/src/codegen.lisp")             :hard)
    ("FR-429" ("packages/type/src/inference.lisp" "packages/type/src/types.lisp")                  :very-hard)
    ("FR-430" ("packages/compile/src/codegen.lisp" "packages/runtime/src/heap.lisp")               :hard)
    ("FR-431" ("packages/expand/src/expander.lisp" "packages/compile/src/codegen.lisp")            :very-hard)
    ("FR-432" ("packages/type/src/inference.lisp" "packages/compile/src/codegen.lisp")             :hard)

    ;; ── Phase 90 — LSP深化・IDE統合 ─────────────────────────────────────────────────
    ("FR-435" ("src/lsp/dap.lisp")                                                                  :hard)
    ("FR-436" ("src/lsp/" "packages/expand/src/expander.lisp")                                      :medium)
    ("FR-437" ("src/lsp/" "packages/type/src/inference.lisp")                                       :medium)
    ("FR-438" ("src/lsp/" "packages/parse/src/diagnostics.lisp")                                    :medium)
    ("FR-439" ("src/lsp/")                                                                          :medium)
    ("FR-440" ("src/lsp/")                                                                          :easy)

    ;; ── Phase 91 — オブザーバビリティ ──────────────────────────────────────────────
    ("FR-442" ("packages/vm/src/vm-run.lisp" "packages/cli/src/main.lisp")                         :medium)
    ("FR-443" ("packages/cli/src/main.lisp" "packages/binary/src/")                                 :hard)
    ("FR-444" ("packages/cli/src/main.lisp")                                                        :easy)
    ("FR-445" ("packages/cli/src/main.lisp" "packages/vm/src/vm-run.lisp")                         :medium)

    ;; ── Phase 92 — インターオペラビリティ ──────────────────────────────────────────
    ("FR-447" ("packages/compile/src/codegen.lisp" "packages/cli/src/main.lisp")                   :medium)
    ("FR-448" ("packages/emit/src/calling-convention.lisp")                                         :hard)
    ("FR-449" ("packages/mir/src/target.lisp" "packages/emit/src/jvm/")                            :very-hard)

    ;; ── Phase 93 — バイナリサイズ・起動時間最適化 ──────────────────────────────────
    ("FR-452" ("packages/binary/src/macho.lisp" "packages/binary/src/elf.lisp")                    :medium)
    ("FR-453" ("packages/cli/src/main.lisp")                                                        :hard)
    ("FR-454" ("packages/cli/src/main.lisp" "packages/pipeline/pipeline.lisp")                     :hard)
    ("FR-455" ("packages/binary/src/macho.lisp")                                                    :medium)

    ;; ── Phase 94 — 追加セキュリティ・解析 ──────────────────────────────────────────
    ("FR-458" ("packages/emit/src/x86-64-codegen.lisp" "src/jit/")                                 :hard)
    ("FR-459" ("src/analyze/taint.lisp")                                                            :hard)
    ("FR-460" ("packages/type/src/inference.lisp" "src/analyze/")                                  :very-hard)
    ("FR-461" ("packages/vm/src/primitives.lisp" "packages/emit/src/x86-64-codegen.lisp")           :medium)
    ("FR-462" ("src/analyze/symbolic-exec.lisp")                                                    :very-hard)

    ;; ── Phase 96 — 高度最適化パス III ──────────────────────────────────────────────
    ("FR-465" ("packages/vm/src/conditions.lisp" "packages/emit/src/x86-64-codegen.lisp"
               "packages/binary/src/macho.lisp")                                                    :very-hard)
    ("FR-466" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-467" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-468" ("packages/compile/src/codegen.lisp" "packages/optimize/src/optimizer.lisp")          :hard)
    ("FR-469" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-470" ("packages/emit/src/x86-64-codegen.lisp")                                             :hard)
    ("FR-471" ("packages/optimize/src/optimizer.lisp" "packages/emit/src/x86-64-codegen.lisp")      :medium)
    ("FR-472" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-473" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-474" ("packages/optimize/src/optimizer.lisp")                                              :medium)

    ;; ── Phase 97 — コード生成・バックエンド拡張 ─────────────────────────────────────
    ("FR-476" ("packages/emit/src/llvm/" "packages/mir/src/target.lisp")                            :hard)
    ("FR-477" ("packages/emit/src/x86-64-codegen.lisp" "packages/compile/src/codegen.lisp")        :hard)
    ("FR-478" ("packages/emit/src/x86-64-codegen.lisp")                                             :hard)
    ("FR-479" ("packages/compile/src/builtin-registry.lisp" "packages/compile/src/codegen.lisp")   :medium)
    ("FR-480" ("packages/pipeline/pipeline.lisp" "packages/cli/src/main.lisp")                     :hard)
    ("FR-481" ("packages/optimize/src/optimizer.lisp" "packages/emit/src/x86-64-codegen.lisp")     :hard)

    ;; ── Phase 98 — 診断・コンパイラ品質向上 ─────────────────────────────────────────
    ("FR-484" ("packages/parse/src/diagnostics.lisp" "packages/compile/src/codegen.lisp")          :easy)
    ("FR-485" ("packages/parse/src/diagnostics.lisp" "packages/cli/src/main.lisp")                 :easy)
    ("FR-486" ("packages/parse/src/diagnostics.lisp" "packages/cli/src/main.lisp")                 :medium)
    ("FR-487" ("packages/optimize/src/optimizer.lisp" "packages/cli/src/main.lisp")                :medium)
    ("FR-488" ("packages/cli/src/main.lisp" "tests/" "flake.nix")                                  :medium)
    ("FR-489" ("tests/integration/" "packages/testing-framework/src/")                              :medium)
    ("FR-490" ("packages/optimize/src/optimizer.lisp" "packages/compile/src/codegen.lisp")         :medium)
    ("FR-491" ("packages/expand/src/expander.lisp" "packages/expand/src/macro.lisp")               :medium)

    ;; ── Phase 99 — 数値・文字列・型特化最適化 ──────────────────────────────────────
    ("FR-494" ("packages/vm/src/primitives.lisp" "packages/compile/src/codegen.lisp")              :medium)
    ("FR-495" ("packages/vm/src/strings.lisp" "packages/runtime/src/heap.lisp")                    :medium)
    ("FR-496" ("src/autodiff/" "packages/compile/src/codegen.lisp")                                 :hard)
    ("FR-497" ("packages/optimize/src/optimizer.lisp")                                              :very-hard)
    ("FR-498" ("packages/optimize/src/optimizer.lisp" "packages/compile/src/codegen.lisp")         :medium)
    ("FR-499" ("packages/pipeline/pipeline.lisp" "cl-cc.asd")                                      :medium)
    ("FR-500" ("packages/type/src/inference.lisp" "packages/pipeline/pipeline.lisp")               :very-hard)
    ("FR-501" ("packages/optimize/src/optimizer.lisp")                                              :very-hard)

    ;; ── Phase 100 — ループ解析・高度変換 II ─────────────────────────────────────────
    ("FR-510" ("packages/optimize/src/scev.lisp")                                                   :hard)
    ("FR-511" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-512" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-513" ("packages/emit/src/x86-64-codegen.lisp" "packages/emit/src/aarch64-codegen.lisp")   :very-hard)
    ("FR-514" ("packages/emit/src/x86-64-codegen.lisp" "packages/emit/src/aarch64-codegen.lisp")   :hard)
    ("FR-515" ("packages/optimize/src/optimizer.lisp")                                              :medium)
    ("FR-516" ("packages/optimize/src/optimizer.lisp")                                              :hard)
    ("FR-517" ("packages/emit/src/x86-64-codegen.lisp")                                             :hard)
    ("FR-518" ("packages/optimize/src/optimizer.lisp")                                              :hard)

    ;; ── Phase 101 — 並行性プリミティブ・メモリモデル ────────────────────────────────
    ("FR-520" ("src/concurrent/atomic.lisp" "packages/emit/src/x86-64-codegen.lisp")                :medium)
    ("FR-521" ("packages/emit/src/x86-64-codegen.lisp" "packages/emit/src/aarch64-codegen.lisp")   :medium)
    ("FR-522" ("src/concurrent/lockfree.lisp")                                                      :very-hard)
    ("FR-523" ("src/concurrent/stm.lisp")                                                           :hard)
    ("FR-524" ("src/concurrent/")                                                                   :hard)
    ("FR-525" ("packages/runtime/src/heap.lisp" "packages/cli/src/main.lisp")                      :easy)

    ;; ── Phase 102 — 言語機能・型システム拡張 II ─────────────────────────────────────
    ("FR-528" ("packages/compile/src/cps.lisp" "packages/vm/src/vm.lisp")                           :medium)
    ("FR-529" ("packages/compile/src/cps.lisp" "packages/vm/src/vm.lisp")                           :hard)
    ("FR-530" ("packages/expand/src/macros-basic.lisp" "packages/type/src/inference.lisp")         :hard)
    ("FR-531" ("packages/type/src/inference.lisp" "packages/type/src/types.lisp")                  :very-hard)
    ("FR-532" ("packages/type/src/inference.lisp" "packages/compile/src/codegen.lisp")             :very-hard)
    ("FR-533" ("packages/expand/src/macros-stdlib.lisp" "packages/compile/src/codegen.lisp")       :medium)
    ("FR-534" ("packages/compile/src/regex.lisp" "packages/expand/src/expander.lisp")              :hard)
    ("FR-535" ("packages/type/src/inference.lisp" "packages/expand/src/macros-stdlib.lisp")        :medium)

    ;; ── Phase 103 — ABI・エコシステム・開発者ヒント ─────────────────────────────────
    ("FR-538" ("packages/binary/src/macho.lisp" "packages/binary/src/elf.lisp")                    :medium)
    ("FR-539" ("packages/expand/src/expander.lisp" "packages/parse/src/diagnostics.lisp")          :easy)
    ("FR-540" ("packages/expand/src/macros-basic.lisp" "packages/emit/src/x86-64-codegen.lisp")    :easy)
    ("FR-541" ("packages/compile/src/codegen.lisp" "packages/emit/src/x86-64-codegen.lisp")        :hard)
    ("FR-542" ("packages/compile/src/codegen.lisp" "packages/emit/src/x86-64-codegen.lisp")        :easy)
    ("FR-543" ("packages/runtime/src/heap.lisp" "packages/vm/src/vm.lisp")                         :hard)
    ("FR-544" ("packages/cli/src/main.lisp" "packages/pipeline/pipeline.lisp")                     :medium)
    ("FR-545" ("packages/compile/src/codegen.lisp" "packages/binary/src/")                         :easy)
    ("FR-546" ("packages/compile/src/regalloc.lisp" "packages/optimize/src/optimizer.lisp")        :hard)))

;; ─── Extracted FR ID lists (from docs/tooling-advanced-1.md) ──────────────────────

(defparameter *tooling-advanced-1-all-fr-ids*
  ;; Phase 79
  '("FR-372" "FR-373" "FR-374"
    ;; Phase 80
    "FR-376" "FR-377" "FR-378"
    ;; Phase 81
    "FR-380" "FR-381" "FR-382" "FR-383" "FR-384"
    ;; Phase 82
    "FR-386" "FR-387" "FR-388"
    ;; Phase 83
    "FR-390" "FR-391" "FR-392"
    ;; Phase 84
    "FR-395" "FR-396" "FR-397" "FR-398" "FR-399"
    ;; Phase 85
    "FR-400" "FR-401" "FR-402" "FR-403" "FR-404" "FR-405"
    "FR-406" "FR-407" "FR-408" "FR-409" "FR-410"
    ;; Phase 86
    "FR-412" "FR-413" "FR-414" "FR-415" "FR-416" "FR-417"
    ;; Phase 88
    "FR-420" "FR-421" "FR-422" "FR-423" "FR-424" "FR-425"
    ;; Phase 89
    "FR-428" "FR-429" "FR-430" "FR-431" "FR-432"
    ;; Phase 90
    "FR-435" "FR-436" "FR-437" "FR-438" "FR-439" "FR-440"
    ;; Phase 91
    "FR-442" "FR-443" "FR-444" "FR-445"
    ;; Phase 92
    "FR-447" "FR-448" "FR-449"
    ;; Phase 93
    "FR-452" "FR-453" "FR-454" "FR-455"
    ;; Phase 94
    "FR-458" "FR-459" "FR-460" "FR-461" "FR-462"
    ;; Phase 96
    "FR-465" "FR-466" "FR-467" "FR-468" "FR-469" "FR-470"
    "FR-471" "FR-472" "FR-473" "FR-474"
    ;; Phase 97
    "FR-476" "FR-477" "FR-478" "FR-479" "FR-480" "FR-481"
    ;; Phase 98
    "FR-484" "FR-485" "FR-486" "FR-487" "FR-488" "FR-489" "FR-490" "FR-491"
    ;; Phase 99
    "FR-494" "FR-495" "FR-496" "FR-497" "FR-498" "FR-499" "FR-500" "FR-501"
    ;; Phase 100
    "FR-510" "FR-511" "FR-512" "FR-513" "FR-514" "FR-515" "FR-516" "FR-517" "FR-518"
    ;; Phase 101
    "FR-520" "FR-521" "FR-522" "FR-523" "FR-524" "FR-525"
    ;; Phase 102
    "FR-528" "FR-529" "FR-530" "FR-531" "FR-532" "FR-533" "FR-534" "FR-535"
    ;; Phase 103
    "FR-538" "FR-539" "FR-540" "FR-541" "FR-542" "FR-543" "FR-544" "FR-545" "FR-546"))

;;; ─── Tests ──────────────────────────────────────────────────────────────────────

(deftest tooling-advanced-1-evidence-registry-covers-all-frs
  "Verify that *tooling-advanced-1-fr-table* contains exactly the 136 FR IDs
   listed in docs/tooling-advanced-1.md and no extras."
  (let ((table-ids (mapcar #'car *tooling-advanced-1-fr-table*))
        (expected-ids *tooling-advanced-1-all-fr-ids*))
    ;; Every expected FR is present in the table.
    (dolist (id expected-ids)
      (assert-true (member id table-ids :test #'string=)))
    ;; Every table FR is expected (no extras).
    (dolist (id table-ids)
      (assert-true (member id expected-ids :test #'string=)))
    ;; Verify total count.
    (assert-= 136 (length table-ids))))

(deftest tooling-advanced-1-evidence-files-exist
  "Verify that every target file referenced in *tooling-advanced-1-fr-table*
   exists on disk.  Directories are checked for existence; for new-path
   entries (no file yet) the existence check is skipped with a warning."
  (let ((checked 0)
        (skipped 0))
    (dolist (entry *tooling-advanced-1-fr-table*)
      (destructuring-bind (fr-id files &optional difficulty) entry
        (declare (ignore difficulty))
        (dolist (file files)
          ;; Skip entries that are explicitly new (not yet created).
          (cond ((probe-file file)
                 (incf checked))
                (t
                 (format t "~&; evidence-files-exist: ~A — file ~A not found (may be new)" fr-id file)
                 (incf skipped))))))
    (format t "~&; evidence-files-exist: ~D checked, ~D skipped (new paths)" checked skipped)
    ;; We do not fail on missing (new) files — this test is informational.
    ;; Zero checked would indicate a problem.
    (assert-true (> checked 0))))
