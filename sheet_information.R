# ===========================================================
# Project: Parkinson's Disease Place of Death Analysis
# Script: sheet_information.R
# Authors: Anna and David Pedrosa
# Date: 2025-07-01
# Version: 1.1
# Description: This function returns a list of specific commands and information
#              (including the sheet name, regression formula, plot title, and groups)
#              for the meta-analysis on a given subitem of the data.
#
# Change log:
#   Version 1.2 (2025-24-03): Typos corrected.
#   Version 1.1 (2025-01-03): Improved documentation for the function.
# ===========================================================

# -------------------------------
# 1. Define the function get_specific_information
# -------------------------------

get_specific_information <- function(x) {
  if (x == "gender") {
    list_sheets <- list(
      name   = "gender",
      mods   = "~ hospitalbeds + ltcbeds",
      title  = "Meta-analysis: odds that men die in hospital",
      groups = c("Male", "Female")
    )
  } else if (x == "city") {
    list_sheets <- list(
      name   = "city",
      mods   = "~ hospitalbeds + ltcbeds",  # mods = c() can be used if no moderators are desired
      title  = "Meta-analysis: odds that people from cities die in hospital",
      groups = c("City", "Other")
    )
  } else if (x == "education") {
    list_sheets <- list(
      name   = "education",
      mods   = "~ hospitalbeds + ltcbeds",  # mods = c() can be used if no moderators are desired
      title  = "Meta-analysis: odds that people with higher education die in hospital",
      groups = c("Higher education", "Lower education")
    )
  } else if (x == "marital_status") {
    list_sheets <- list(
      name   = "marital_status",
      mods   = "~ hospitalbeds + ltcbeds",  # mods = c() can be used if no moderators are desired
      title  = "Meta-analysis: odds that married people die in hospital",
      groups = c("Married", "Unmarried")
    )
  } else if (x == "age") {
    list_sheets <- list(
      name   = "age",
      mods   = "~ hospitalbeds + ltcbeds",  # mods = c() can be used if no moderators are desired
      title  = "Meta-analysis: odds that individuals under 85 years die in Hospital",
      groups = c("<85 years", ">85 years")
    )
  } else if (x == "palliative_homecare") {
    list_sheets <- list(
      name   = "palliative_homecare",
      mods   = c(),
      title  = "Meta-analysis: odds that the presence of palliative homecare increases mortalities in hospitals",
      groups = c("Presence of palliative homecare", "not")
    )
  } else {
    stop("Invalid input")
  }
  
  return(list_sheets)
}

