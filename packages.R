# ===========================================================
# Project: Parkinson's Disease Place of Death Analysis
# Script: packages.R
# Authors: Anna and David Pedrosa
# Date: 2025-07-01
# Version: 1.0
# Description: This script checks for the required packages, installs any that
#              are missing, and then loads them.
#
# Change log:
#   Version 1.0 (2025-07-01): Initial version.
# ===========================================================

# -------------------------------
# 1. Define the list of required packages for this project
# -------------------------------

required_packages <- c(
  "broom",
  "clubSandwich",
  "dplyr",
  "lme4",
  "metafor",
  "readxl",
  "tidyverse"
)

# -------------------------------
# 2. Check if the required packages are installed; if not, install them and load
# -------------------------------

package.check <- lapply(
  required_packages,
  FUN = function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
)

