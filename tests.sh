#!/bin/sh

set -e

BUILD=${BUILD:-build}

echo "Test 1"
$BUILD/test_sexptran cases/config.sexp
echo

echo "Test 2"
$BUILD/simplest_example
