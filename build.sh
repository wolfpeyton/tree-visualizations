#!/bin/sh
# Compiles the Elm app into elm.js (loaded by index.html)
set -e
cd "$(dirname "$0")"
elm make Main.elm --output=elm.js
