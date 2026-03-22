#!/bin/bash
# scripts/bootstrap-stage1.sh
#
# Stage 1: First Self-Compilation
#
# Stage 1バイナリを使用してcl-cc自身をコンパイル

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo "----------------------------------"
echo "Stage 1: Self-Compilation"
echo "----------------------------------"
echo ""

# ビルドディレクトリを作成
mkdir -p build/stage2

# Stage 1を使用してブートストラップを実行
echo "Running Stage 1 bootstrap..."
sbcl --load cl-cc.asd \
      --eval '(asdf:load-system :cl-cc)' \
      --eval '(in-package :cl-cc)' \
      --eval '(bootstrap-stage-1 "build/clcc-stage1")' \
      --eval '(quit)'

echo ""
echo "----------------------------------"
echo "Stage 1 Complete"
echo "----------------------------------"
echo ""
echo "✅ Stage 2 binary created: build/clcc-stage2"
echo ""
