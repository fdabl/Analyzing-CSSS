# Analyzing Complex Collaborations
This repository contains data about the annual Complex Systems Summer School from 2005 - 2018. The analysis code is in exps/analysis.Rmd which compiles to an html page. You can view it by opening it with your browser.

Base code necessary for this analysis can be found in the ACSSS R package that is included with this project. To install this library into your local environment, please use `install.packages("<basepath>/Analyzing-CSSS/ACSSS/")`.

Derived data and figures can be produced using the `Makefile` in this root directory. To install or reload the ACSSS R package, simple navigate to the project directory in terminal, and run `make reload`. To produce the derived data and figures on your own machine, run `make all`. Some scripts will require that `ggplot2` and `dplyr` are installed. 
