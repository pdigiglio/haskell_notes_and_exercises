#!/bin/bash

typeset -ar files=(
week4/hw.hs 
week5/*.hs
)

haddock --html --hyperlinked-source ${files[@]} --odir doc
