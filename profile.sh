#!/bin/bash

stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -O"
sqlite3 properties.db "DELETE FROM properties"
stack exec -- rea-extract +RTS -p
hp2ps rea-extract.hp
