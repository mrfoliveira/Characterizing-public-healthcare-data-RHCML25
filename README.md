# Characterizing Publicly Available Tabular Health Data Sets for Responsible Machine Learning

This repository contains the research compendium of the [RHCML 2025 workshop](https://rhcml.github.io/) short paper:

Oliveira, M., Soares, C. (2026). Characterizing Publicly Available Tabular Health Datasets for Responsible Machine Learning. In: Koprinska, I., Mendes-Moreira, J., Branco, P. (eds) Machine Learning and Principles and Practice of Knowledge Discovery in Databases. ECML PKDD 2025. Communications in Computer and Information Science, vol 2842. Springer, Cham. [https://doi.org/10.1007/978-3-032-19105-2_25](https://doi.org/10.1007/978-3-032-19105-2_25)

You can find a pre-print [here](https://rhcml.github.io/assets/paper438.pdf).

## Reproducing experiments

To obtain all results and generate an HTML report containing all figures and tables in the article, run the following lines from the directory:

```
source("R/utils.R")
source("R/scoring_functions.R")
knitr::knit("inst/report.Rmd")
```

### BibTeX Reference 

@InProceedings{10.1007/978-3-032-19105-2_25,
author="Oliveira, Mariana
and Soares, Carlos",
editor="Koprinska, Irena
and Mendes-Moreira, Jo{\~a}o
and Branco, Paula",
title="Characterizing Publicly Available Tabular Health Datasets for Responsible Machine Learning",
booktitle="Machine Learning and Principles and Practice of Knowledge Discovery in Databases",
year="2026",
publisher="Springer Nature Switzerland",
address="Cham",
pages="351--359",
isbn="978-3-032-19105-2",
doi="10.1007/978-3-032-19105-2\_25"
}

