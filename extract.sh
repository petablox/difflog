#!/usr/bin/env bash

# Example run: ./extract.sh ObjectPointer.py

grep 'parse_rules' "$@" | \
sed 's/.*\[\"\(.*\).\"\].*/\1/' | \
sed "s/.*\[\'\(.*\).\'\].*/\1/" | \
sed 's/ :- /, /' | \
nl | sed 's/^ *//' | sed 's/\t/)), /' | \
sed 's/^/Value(0.5, Token(/' | \
nl | sed 's/^ *//' | sed 's/\t/, /' | \
sed 's/^/Rule(/' | sed 's/$/)/'
