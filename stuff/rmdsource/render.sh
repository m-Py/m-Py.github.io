#!/bin/sh

R -e 'source("renderAll.r")'

cd ..
git add .
cd -
