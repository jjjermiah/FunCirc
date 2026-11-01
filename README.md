# FunCirc
FunCirc is an interactive Shiny app designed as a comprehensive resource for functional circular RNAs (circRNAs). This database integrates data from multiple circRNA screening studies, providing researchers with tools to query and visualize circRNA essentiality and clinical expression data across various cancer types and tissue subtypes.

## Overview

Features: 
- **Interactive Data Exploration**: Query essential circRNAs based on various studies, cell lines, and tissue types.
- **Visualization of circRNA Essentiality**: Display essentiality plots from different studies to highlight critical circRNAs across cancer subtypes.
- **Clinical Expression Analysis**: Explore circRNA expression in clinical datasets, including in-house breast cancer cohorts and prostate cancer genomes.

## Table of Contents
- [Overview](#overview)
- [Citations](#citations)
- [Requirements](#requirements)
- [Installation](#installation)
- [Setup](#setup)
- [Running the Application](#running-the-application)
- [Troubleshooting](#troubleshooting)

## Citations

This project utilizes datasets and methodologies from multiple studies. If you use **FunCirc** in your research, please consider citing the following sources:

- **Liu et al.**: Liu L, Neve M, Perlaza-Jimenez L, Xi X, Purcell J, Hawdon A, et al. *Systematic loss-of-function screens identify pathway-specific functional circular RNAs.* Nature Cell Biology. 2024 Aug;26(8):1359–72. Available from: [https://www.nature.com/articles/s41556-024-01467-y](https://www.nature.com/articles/s41556-024-01467-y)

- **Li et al.**: Li S, Li X, Xue W, Zhang L, Yang LZ, Cao SM, et al. *Screening for functional circular RNAs using the CRISPR–Cas13 system.* Nature Methods. 2020 Dec 7;18(1):51–9. Available from: [https://www.nature.com/articles/s41592-020-01011-4](https://www.nature.com/articles/s41592-020-01011-4)

- **Chen et al.**: Chen S, Huang V, Xu X, Livingstone J, Soares F, Jeon J, et al. *Widespread and Functional RNA Circularization in Localized Prostate Cancer.* Cell. 2019 Feb 1;176(4):831-843.e22. Available from: [https://www.cell.com/cell/fulltext/S0092-8674(19)30058-3?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0092867419300583%3Fshowall%3Dtrue](https://www.cell.com/cell/fulltext/S0092-8674(19)30058-3?_returnURL=https%3A%2F%2Flinkinghub.elsevier.com%2Fretrieve%2Fpii%2FS0092867419300583%3Fshowall%3Dtrue)

- **CPCG**: Fraser M, Sabelnykova VY, Yamaguchi TN, Heisler LE, Livingstone J, Huang V, et al. *Genomic hallmarks of localized, non-indolent prostate cancer.* Nature. 2017 Jan;541(7637):359-364. Available from: [https://doi.org/10.1038/nature20788](https://doi.org/10.1038/nature20788)

- **Arul et al.**: Vo JN, Cieslik M, Zhang Y, Shukla S, Xiao L, Zhang Y, et al. *The Landscape of Circular RNA in Cancer.* Cell. 2019 Feb 1;176(4):869-881.e13. Available from: [https://pubmed.ncbi.nlm.nih.gov/30735636/](https://pubmed.ncbi.nlm.nih.gov/30735636/)

## Requirements

### Software
- **R** (version >= 4.0) – [Download and Install R](https://cran.r-project.org/)
- **RStudio** (recommended) – [Download and Install RStudio](https://rstudio.com/products/rstudio/download/)

### R Packages
FunCirc relies on several R packages. They are listed below and will be automatically installed if missing during setup.

- `shiny`, `shinydashboard`, `shinyWidgets`, `dplyr`, `tidyr`, `data.table`, `DT`, `ggplot2`, `BoutrosLab.plotting.general`, `janitor`, `readxl`, `rtracklayer`, `tibble`, `GenomicRanges`, `ggrepel`

## Installation

### Clone the Repository
First, clone this repository to your local machine:
```bash
git clone https://github.com/HansenHeLab/FunCirc.git
cd FunCirc
```
Note: make sure you have git **Git LFS** installed: 
```bash
brew install git-lfs
git lfs install
```

## Setup
### Install Dependencies and Data
Open RStudio, set the working directory to the FunCirc folder 
```RStudio console
setwd("path/to/FunCirc")
```

Run the following in the console to install any missing R packages and download the data:
```RStudio console
source("dependencies.R")
```

Note: Ensure that all_data.RData is located in the data folder within the main project directory. The file path in dependencies.R and the source command might need to be edited. This file should contain all required data for the app to function. If you use a different path, update the path in dependencies.R. 

## Running the Application

To start the app, in the console run the command

```RStudio
library(shiny)
runApp("app.R")  
```

## Troubleshooting 

### Where to Place Setup Files

- **Scripts**: `app.R` and `dependencies.R`should be in the main repository.
- **Folders**: `www` contains images, and `data` contains all relevant data for the app. 
- **Data**: Place data file in a `data` folder to avoid modifying the main script paths directly.
- **Paths**: Working directory should be changed to FunCirc directory, check file paths to ensure they are beign referenced properly. 




