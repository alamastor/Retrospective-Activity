#!/bin/bash
rm dist/*
cp src/index.html dist
cp src/style.css dist
elm make src/Main.elm --output=dist/elm.js
