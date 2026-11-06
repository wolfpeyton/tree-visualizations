#!/bin/sh
# Compiles the app into elm.js (loaded by index.html)
set -e
cd "$(dirname "$0")"
elm make Arithmetic/MathDisplay.elm --output=elm.js
