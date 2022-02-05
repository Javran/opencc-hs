#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.0.1.0.0-1)
cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d $dir/*-docs.tar.gz
