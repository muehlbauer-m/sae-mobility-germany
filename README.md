# Small Area Estimation of Multimodal Mobility in Germany

This repository contains the R code accompanying a paper on multimodal small area estimation of district-level person-kilometers traveled per day in Germany.

## Overview

The methodology includes:
- **Direct estimation** using Horvitz-Thompson-Hajek estimators with Taylor linearization variance
- **Three-phase variable selection**: LASSO main effect screening, ranked sparsity LASSO (sparseR) for interactions and polynomials, and cross-validation on the Fay-Herriot model
- **Fay-Herriot small area estimation** with log transformation across 1,590 district x mode domains
- **Interactive Shiny dashboard** for exploring district-level results

## Repository Structure

```
sae-mobility-germany/
├── run_analysis.r                  # Main pipeline orchestrator (Steps 1-10)
├── R/
│   ├── data_preparation/           # Steps 1-4: Data cleaning, census/INKAR acquisition
│   ├── direct_estimation/          # Step 5: Horvitz-Thompson-Hajek direct estimates
│   ├── modeling/                   # Steps 6-9: LASSO, sparseR, cross-validation, FH models
│   ├── plotting/                   # Step 10: Figures and tables for the manuscript
│   └── auxiliaries/                # Helper functions (themes, plots, tables, data wrangling)
├── ShinyApp/
│   ├── app.R                       # Interactive Shiny dashboard
│   ├── prepare_data.R              # Generates pre-computed data from model outputs
│   ├── mid_shiny_data.RData        # Pre-computed estimates (included for immediate use)
│   └── data/                       # Reference tables (variable sets, census metadata)
├── data/                           # Input data directory (not included, see below)
└── LICENSE
```

## Running the Analysis

### Prerequisites

- R >= 4.2.0
- Required packages (installed automatically via `pacman`):

| Category | Packages |
|----------|----------|
| Data wrangling | `tidyverse`, `data.table`, `readxl`, `reshape`, `reshape2` |
| Survey estimation | `survey`, `srvyr`, `laeken` |
| Variable selection | `glmnet`, `caret`, `sparseR`, `rsample` |
| SAE modeling | `emdi` |
| Parallel computing | `doParallel`, `foreach`, `parallel` |
| Data acquisition | `restatis`, `usethis`, `bonn` |
| Visualization | `ggplot2`, `ggpubr`, `viridis`, `colorspace`, `paletteer`, `patchwork`, `sf`, `latex2exp` |
| Tables | `xtable`, `knitr`, `kableExtra` |
| Shiny app | `shiny`, `bslib`, `cowplot`, `ggiraph`, `DT` |

### Pipeline Execution

From the project root:

```r
source("run_analysis.r")
```

This runs all 10 steps sequentially (total runtime: ~3-5 hours, mostly variable selection). Each step can also be run individually by sourcing the corresponding script.

### Data Requirements

The MiD 2017 survey microdata are not publicly distributed. See [`data/README.md`](data/README.md) for details on obtaining the required datasets:
- **MiD 2017**: Application to BMDV (German Federal Ministry for Digital and Transport)
- **Destatis census tables**: Acquired automatically via the GENESIS API (`restatis` package)
- **INKAR indicators**: Acquired automatically via the BBSR API (`bonn` package)
- **District boundaries**: BKG VG2500 shapefile (freely available)

### Shiny Dashboard

The interactive dashboard is self-contained and can be used **without any input data or prior pipeline execution**. All pre-computed estimates and reference tables are included.

**Option 1 -- Use the hosted version (no installation required):**

https://muehlbauer-m.shinyapps.io/sae-mobility-germany/

**Option 2 -- Run locally:**

1. Install [R](https://cran.r-project.org/) (>= 4.2.0)
2. Open R and install the required packages:
   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "sf", "tidyverse",
                       "cowplot", "viridis", "colorspace", "ggiraph",
                       "patchwork", "DT"))
   ```
3. Run the app:
   ```r
   shiny::runApp("ShinyApp")
   ```

To regenerate the Shiny data from fresh model outputs (requires completed pipeline):

```r
source("ShinyApp/prepare_data.R")  # from project root
```

## License

MIT License. See [LICENSE](LICENSE).

## Citation

If you use this code, please cite the accompanying paper (citation details to be added upon publication).
