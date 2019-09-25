#!/bin/bash

cwd="$PWD"

Rscript -e "devtools::document(\"$cwd/ACSSS\")"
Rscript -e "devtools::install(\"$cwd/ACSSS\")"
