#!/bin/bash
stack build
PROJECT_DIR="/home/hrk/Devel/github.com/RoboZonky/natural-strategy-setup"
find $PROJECT_DIR -name '*.elm' | grep -v elm-stuff > elmFiles.txt
stack exec elm-module-analysis elmFiles.txt #produces dependencies.dot
tred dependencies.dot | dot -Tpng -odependencies.png
echo "Dependency analysis result has been written to dependencies.png"
