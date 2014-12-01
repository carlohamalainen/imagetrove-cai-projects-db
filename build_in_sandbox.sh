#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

rm -fr .cabal-sandbox cabal.sandbox.config dist

rm -fr imagetrove-uploader

git clone git@github.com:carlohamalainen/imagetrove-uploader.git

cabal sandbox init

cabal sandbox add-source imagetrove-uploader

cabal install --haddock-hyperlink-source --dependencies-only
cabal install --haddock-hyperlink-source
