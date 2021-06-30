#!/usr/bin/env bash

TEST_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN=$TEST_HOME/bug

function run_test() {
  diff -q <($BIN $1) <(expr $1 \* $1) >/dev/null
  return $?
}

case $1 in
p1) run_test 1 && exit 0 ;;
p2) run_test 12 && exit 0 ;;
p3) run_test 123 && exit 0 ;;
p4) run_test 1234 && exit 0 ;;
p5) run_test 12345 && exit 0 ;;
n1) run_test 487 && exit 0 ;;
esac
exit 1
