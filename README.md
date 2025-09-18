# Characterizing Publicly Available Tabular Health Data Sets for Responsible Machine Learning

This repository contains the research compendium of the workshop short paper:

Oliveira, M., & Soares, Carlos. Characterizing Publicly Available Tabular Health Data Sets for Responsible Machine Learning. Accepted at Workshop on Responsible Healthcare using Machine Learning ([RHCML](https://rhcml.github.io/)) co-located with ECML-PKDD 2025.

You can find a pre-print [here](https://rhcml.github.io/assets/paper438.pdf).

## Reproducing experiments

To obtain all results and generate an HTML report containing all figures and tables in the article, run the following lines from the directory:

```
source("R/utils.R")
source("R/scoring_functions.R")
knitr::knit("inst/report.Rmd")
```
