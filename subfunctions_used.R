# ===========================================================
# Project: Parkinson's Disease Place of Death Analysis
# Script: subfunctions_used.R
# Authors: Anna and David Pedrosa
# Date: 2025-07-01
# Version: 1.0
# Description: This script provides the necessary subfunctions to run the main
#              analysis (data_analysis.R), including functions to read Excel data
#              and generate plot labels with meta-analysis estimates.
#
# Change log:
#   Version 1.0 (2025-07-01): Initial version.
# ===========================================================

# -------------------------------
# 1. Read MS Excel Files
# -------------------------------
read_POD_Hospital_data <- function(sheet) {
  read_excel(
    path = file.path(data_dir, "datatables_POD_Hospital.xlsx"),
    sheet = sheet
  )
}

# -------------------------------
# 2. Generate Label with Meta-Analysis Estimates (No Moderators)
# -------------------------------
mlabfun <- function(text, x) {
  list(bquote(paste(
    .(text),
    " (Q = ", .(fmtx(x$QE, digits = 2)),
    ", df = ", .(x$k - x$p), ", ",
    .(fmtp(x$QEp, digits = 3, pname = "p", add0 = TRUE, sep = TRUE, equal = TRUE)), "; ",
    I^2, " = ", .(fmtx(x$I2, digits = 1)), "%, ",
    tau^2, " = ", .(fmtx(x$tau2, digits = 2)), ")"
  )))
}

# -------------------------------
# 3. Generate Label with Meta-Analysis Estimates (With Moderators)
# -------------------------------
mlabfun_MOD <- function(text, x) {
  list(bquote(paste(
    .(text),
    " (Q = ", .(fmtx(x$QM, digits = 2)),
    ", df = ", .(x$k - x$p), ", ",
    .(fmtp(x$QMp, digits = 3, pname = "p", add0 = TRUE, sep = TRUE, equal = TRUE)), "; ",
    I^2, " = ", .(fmtx(x$I2, digits = 1)), "%, ",
    tau^2, " = ", .(fmtx(x$tau2, digits = 2)), ")"
  )))
}

