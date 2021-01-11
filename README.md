# Analyzing Complex Collaborations
This repository contains data and code for the analyses reported in Brown, J., Murray, D., Furlong, K., Coco, E., Dablander, F. ([2020](https://osf.io/preprints/socarxiv/e3z4v)). A Breeding Pool of Ideas: Analyzing Interdisciplinary Collaborations at the Complex Systems Summer School. Specifically, the analysis code is in exps/analysis.Rmd which compiles to a HTML page.

Base code necessary for this analysis can be found in the ACSSS R package that is included with this project. To install this library into your local environment, please use `install.packages("<basepath>/Analyzing-CSSS/ACSSS/")`.

Derived data and figures can be produced using the `Makefile` in this root directory. To install or reload the ACSSS R package, simply navigate to the project directory in terminal, and run `make reload`. To produce the derived data and figures on your own machine, run `make all`. Some scripts will require that `ggplot2` and `dplyr` are installed. 