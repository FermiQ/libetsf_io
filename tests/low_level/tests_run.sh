#!/bin/sh

if `grep -qs "Failed" tests_write.log tests_read.log`; then
  exit 1
else
  exit 0
fi
