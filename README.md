# Matching Employment Accessibility

This repository contains the code and data supporting the "Assessing public transport infrastructure: the role of employment matching in spatial accessibility measures" paper. This has been accepted for publication in *Special Issue: Urban analytics for evidence-based decision-making: from data through theory to better cities* in Journal of Geographical Systems, available at <https://link.springer.com/collections/heicfdchic>.


## Repository Structure

```
matching_employment/
├── R/                        # Main analytical scripts (run in numeric sequence)
├── data/                     # Raw and processed data files
├── output/                   # Figures, tables, and results
├── paper/                    # Manuscript and LaTeX source
├── matching_employment.Rproj # RStudio project file
├── LICENSE                   # CC0‑1.0 public domain license
└── .gitignore
```

## Analysis Workflow

All analysis is implemented in the `R` folder. Scripts are designed to be run **in numeric order**, for example:

```r
source("R/01 DataCleaning.R")
source("R/02 Modeling.R")
source("R/03 Visualizations.R")
```

`R` files starting with the pattern '00' are local functions.

> Ignore underscores in file names; numbering indicates the execution order.

### Requirements

- **R** (4.x or later)  
- **RStudio** (recommended)  
- Common packages:

```r
install.packages(c(
  "tidyverse",
  "data.table",
  "sf",
  "ggplot2",
  "readr",
  "knitr",
  "rmarkdown"
))
```

> Add any additional packages required by specific scripts.  


## Paper

The `paper/` folder contains the manuscript drafts and the accepted version. All figures and tables in the paper are reproducible from the scripts in this repo.


## 📝 License

Distributed under the **CC0‑1.0 License** — public domain dedication.