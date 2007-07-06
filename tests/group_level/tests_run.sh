#!/bin/sh


if `grep -qs "Failed" tests_*.log`; then
  exit 1
else
  exit 0
fi
