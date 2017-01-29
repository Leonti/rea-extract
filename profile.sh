#!/bin/bash

set -e

stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -O"
sqlite3 properties.db "DELETE FROM properties"
stack exec -- rea-extract +RTS -p -h
hp2ps rea-extract.hp
