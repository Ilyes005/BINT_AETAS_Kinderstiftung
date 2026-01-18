---
editor_options: 
  markdown: 
    wrap: 72
---


### AETAS Kinderstiftung

## Authors

Yassine Ben Ahmed\
Ilyes Ihidheb\
Nour Nessah\
Eya Tlili

------------------------------------------------------------------------

## 1. Project Overview

This repository contains the code and documentation for a data analysis
project conducted in cooperation with the **AETAS Kinderstiftung**.

The project analyzes survey data on the experiences of parents and close
caregivers who informed children under the age of 18 about the violent
death (suicide or homicide) of a close person. The analyses focus on
descriptive statistics and visualizations related to perceived stress,
psychosocial support, and communication with children.

The project is part of the *Grundlegendes Praxisprojekt* at LMU Munich
and follows principles of transparent and reproducible data analysis.

------------------------------------------------------------------------

## 2. Data Availability and Confidentiality

**Confidential data – raw data not included**

The raw survey data are **not publicly available** due to ethical and
data protection restrictions.

To reproduce the complete data cleaning pipeline, the raw data must be
provided manually and placed in:

For reproducibility and analysis purposes, **processed datasets** are
included in this repository as `.rds` files and stored in
`data/processed/`

The following processed datasets are provided: - `selected.data.rds` -
`cleaned_data.rds` - `clean_data.rds` - `d.rds`

These files allow all analysis and plotting scripts to be executed
**without access to the raw data**.

------------------------------------------------------------------------

## 3. Repository Structure

```         
.
├─ AETAS.Rproj
├─ README.md
├─ presentation.qmd
├─ customstyle.scss
│
├─ data/
│  ├─ raw/                    # raw input data (not included)
│  └─ processed/              # cleaned and processed datasets (.rds) (not included)
│
├─ scripts/
│  ├─ cleaned.data.R          # code to generate processed datasets from raw data
│  ├─ utils.R                 # package loading, data loading, color palettes
│  ├─ prepare plot data.R     # creation of plot-ready datasets
│  └─ plot.R                  # generation of figures
│
├─ results/                   # generated plots and final HTML presentation
│
├─ renv/
└─ renv.lock                  # fixed package versions for reproducibility
```

------------------------------------------------------------------------

## 4. Software and Environment

-   **R version:** 4.5.1
-   **Dependency management:** `renv`
-   **Recommended IDE:** RStudio

All required R packages and their exact versions are recorded in
`renv.lock`.

------------------------------------------------------------------------

## 5. Environment Setup (Required)

Before running any scripts, the R package environment must be restored.

``` r
renv::restore()
```

------------------------------------------------------------------------

## 6. Script Execution (Without Quarto)

If the scripts are run individually (i.e., without rendering the Quarto
file), they must be executed in the following order. Load "utils.R"
script Load "prepare plot data.R" script Load "plot.R" script

------------------------------------------------------------------------

## 7. Full Data Cleaning Pipeline (Optional)

If the raw data are available and the full data preparation pipeline
should be reproduced, run: "cleaned.data.R" script

------------------------------------------------------------------------

## 8. Report Generation (Quarto)

To generate the final presentation: Open presentation.qmd Render the
file using Quarto (HTML output) The rendered HTML presentation is saved
in the results/ directory.
