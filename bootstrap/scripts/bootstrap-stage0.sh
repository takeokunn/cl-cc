#!/bin/bash
# scripts/bootstrap-stage0.sh
#
# Stage 0: Host Common Lisp Bootstrap
#
# ホストのCommon Lisp（SBCL）を使用してcl-ccを初期コンパイル

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo "----------------------------------"
echo "Stage 0: Host CL Bootstrap"
echo "----------------------------------"
echo ""

# ビルドディレクトリを作成
mkdir -p build/stage1

# SBCLを使用してブートストラップを実行
echo "Running Stage 0 bootstrap..."
sbcl --load cl-cc.asd \
      --eval '(asdf:load-system :cl-cc)' \
      --eval '(in-package :cl-cc)' \
      --eval '(bootstrap-stage-0)' \
      --eval '(quit)'

echo ""
echo "----------------------------------"
echo "Stage 0 Complete"
echo "----------------------------------"
echo ""
echo "✅ Stage 1 binary created: build/clcc-stage1"
echo ""
