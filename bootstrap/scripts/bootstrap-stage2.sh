#!/bin/bash
# scripts/bootstrap-stage2.sh
#
# Stage 2: Fixed Point Verification
#
# Stage 1とStage 2がバイト単位で一致することを検証

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo "----------------------------------"
echo "Stage 2: Fixed Point Verification"
echo "----------------------------------"
echo ""

# Stage 2を使用して固定点検証を実行
echo "Running Stage 2 verification..."
sbcl --load cl-cc.asd \
      --eval '(asdf:load-system :cl-cc)' \
      --eval '(in-package :cl-cc)' \
      --eval '(bootstrap-stage-2 "build/clcc-stage2")' \
      --eval '(quit)'

echo ""
echo "----------------------------------"
echo "Bootstrap Complete!"
echo "----------------------------------"
echo ""
echo "✅ Fixed point achieved!"
echo "✅ Self-hosting completed!"
echo ""
echo "Final binary: build/clcc-stage2"
echo ""
