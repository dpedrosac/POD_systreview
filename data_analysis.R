# ===========================================================
# Project: Parkinson's Disease Place of Death Analysis
# Script: data_analysis.R
# Authors: Anna and David Pedrosa
# Date: 2025-26-02
# Version: 1.3
# Description: This script performs meta-analyses on data concerning the
#              place of death for people suffering from Parkinson's disease.
#
# Change log:
#   Version 1.5 (2025-31-03): Changed typos and folders.
#   Version 1.4 (2025-26-02): Added confidence intervals to meta-analysis.
#   Version 1.3 (2025-07-01): Added functionality, generic filenames, and fixed indentation.
#   Version 1.2 (2024-11-25): Tidied up the preliminary code.
# ===========================================================

# -------------------------------
# 1. Load Packages and Set Directories
# -------------------------------

source("packages.R")          # Load required packages
source("subfunctions_used.R") # Functions: 1. Read Excel data, 2. Estimate I^2, etc.
source("sheet_information.R") # Read sheet information as per function input
cex_plot = 1.25

# Set working directory based on the username
username <- Sys.getenv("USER")

if (username == "dpedr") {
  wdir <- "D:/POD_systReview"
  data_dir <- file.path(wdir, "data")
} else if (username == "david") {
  wdir <- "/media/storage/POD_systReview"
  data_dir <- file.path(wdir, "data")
} else {
  cat("Username unknown\n")
}

setwd(wdir)

# Optional flag for performing additional checks if necessary
# flag_check <- FALSE  # not needed here

# -------------------------------
# 2. Define Sheets and Initialize Result Lists
# -------------------------------

sheets_to_analyse <- c("gender", "city", "education", "marital_status", "age")  
# Note: "palliative_homecare" not working, therefore ignored

# Initialize lists to store meta-analysis results
res_metaMOD_list <- list()
res_meta_list    <- list()

# -------------------------------
# 3. Loop Over Sheets to Analyze Data and Perform Meta-Analyses
# -------------------------------

for (var in sheets_to_analyse) {
  
  # Retrieve sheet-specific information
  list_sheets <- get_specific_information(var)
  
  # Import and clean data
  df <- read_POD_Hospital_data(list_sheets$name) %>%
    mutate(
      across(-2, ~ as.numeric(as.character(.))),  # Converts "p < .001" to NA
      se = sqrt(1 / t_pos + 1 / t_neg + 1 / c_pos + 1 / c_neg)
    )
  
  df_complete <- df %>%
    filter(complete.cases(t_pos, t_neg, c_pos, c_neg)) %>%
    mutate(
      fisher_result = pmap(
        list(t_pos, t_neg, c_pos, c_neg),
        function(tp, tn, cp, cn) {
          fisher.test(matrix(c(tp, tn, cp, cn), nrow = 2))
        }
      ),
      p_values = map_dbl(fisher_result, ~ tidy(.x)$p.value),
      same_publication = if_else(str_detect(author, "Moens"), 1, 0)
    ) %>%
    select(-fisher_result)
  
  df_complete$number <- seq_len(nrow(df_complete))
  
  # Perform meta-analysis conversion using conv.wald
  df_complete <- conv.wald(
    out = OR,
    ci.lb = ci.lb,
    ci.ub = ci.ub,
    pval = p_values,
    n = n_ges,
    data = df_complete,
    transf = log
  )
  
  # Common parameters for meta-analysis
  common_params <- list(
    yi      = df_complete$yi,
    vi      = df_complete$vi,
    random  = ~ 1 | as.factor(df_complete$same_publication) / number,
    verbose = TRUE,
    control = list(optimizer = "optim", optmethod = "Nelder-Mead"),
    data    = df_complete
  )
  
  # Meta-analysis: with or without moderators
  if (!is.null(list_sheets$mods)) {
    res_metaMOD <- do.call(rma, c(common_params, list(mods = as.formula(list_sheets$mods))))
    ylim2 <- -2.5
  } else {
    res_metaMOD <- do.call(rma, common_params)
    ylim2 <- -1.5
  }
  
  # -------------------------------
  # 3a. Generate Forest Plot
  # -------------------------------
  
  output_file <- file.path(wdir, "results",
                           sprintf("fig%.2d.forest_plot.OR_%s.v1.1.png",
                                   which(sheets_to_analyse == var), var))
  png(filename = output_file, width = 2000, height = 1250, units = "px", res = 96)
  
  forest1 <- forest(
    res_metaMOD, 
    xlim      = c(-11, 6),
    ylim      = c(ylim2, res_metaMOD$k + 3),
    atransf   = exp,
    showweights = TRUE,
    addfit    = TRUE,
    addpred   = TRUE,
    order     = ltcbeds,
    xlab      = "Odds ratio [OR]",
    header    = FALSE,
    slab      = NA,
    mlab      = mlabfun("RE Model for all Studies", res_metaMOD),
    ilab      = cbind(
                  df_complete$author,
                  df_complete$year,
                  df_complete$t_pos,
                  df_complete$t_neg,
                  df_complete$c_pos,
                  df_complete$c_neg,
                  df_complete$n_ges
                ),
    ilab.pos  = 4,
    ilab.xpos = c(-11, -8, -7, -6, -5, -4, -3),
    alim      = c(-4, 3),
    at        = log(c(0.25, 1, 4, 20)),
    cex       = 1.25,
    main      = list_sheets$title,
    shade     = TRUE
  )
  
  segments(-7, forest1$ylim[2] - 1, -5.5, forest1$ylim[2] - 1)
  segments(-5, forest1$ylim[2] - 1, -3.5, forest1$ylim[2] - 1)
  
  # Additional plot text for groups and headers
  text(
    c(-6.25, -4.25), 
    res_metaMOD$k + 2.5, 
    font  = 2, 
    cex   = cex_plot, 
    c(list_sheets$groups[1], list_sheets$groups[2])
  )
  text(
    c(-10.55, -7.75, -6.75, -5.75, -4.75, -3.75, -2.75, 4.5),
    res_metaMOD$k + 1.5, 
    cex = cex_plot,
    c("Author(s)", "year", "hosp.", "other", "hosp.", "other",
      expression("n"[tot]), "Weights and OR [95%-CI]")
  )
  
  # Meta-analysis without moderators for comparison
  res_meta <- rma(
    yi,
    vi,
    random  = ~ 1 | as.factor(df_complete$same_publication) / number,
    verbose = TRUE,
    control = list(optimizer = "optim", optmethod = "Nelder-Mead"),
    data    = df_complete
  )
  
  # Store results
  res_metaMOD_list[[var]] <- res_metaMOD
  res_meta_list[[var]]    <- res_meta
  
  if (!is.null(list_sheets$mods)) {
    # Predictions for random-effects model
    sav1 <- predict(res_meta)
    addpoly(res_meta, row = -1, mlab = mlabfun("Random-Effects Model", res_meta))
    
    # Predictions for meta-regression model using average moderators
    sav2 <- predict(
      res_metaMOD,
      newmods = c(mean(df_complete$hospitalbeds), mean(df_complete$ltcbeds)), atransf=exp
    )
    confint(res_meta) # shows confidence intervals
    confint(res_metaMOD)
    addpoly(sav2, row = -2, mlab = "Meta-Regression Model (Adjusted Effect)")
    # Reference: https://www.metafor-project.org/doku.php/tips:computing_adjusted_effects
  }
  
  abline(h = 0)
  dev.off()  # Close forest plot device
  
  cat("Forest plot saved to", output_file, "\n")
  
  
  # -------------------------------
  # 3b. Generate Funnel Plot
  # -------------------------------
  
  output_file <- file.path(wdir, "results",
                           sprintf("suppl.material.fig%.2d.eggers_%s.v1.0.png",
                                   which(sheets_to_analyse == var), var))
  width <- 600
  height <- 800
  
  png(filename = output_file, width = width, height = height, units = "px", res = 96)
  
  # Perform Egger's test for asymmetry
  eggers <- regtest(res_metaMOD)
  
  funnel(
    res_metaMOD,
    cex      = 2,
    cex.axis = 1.5,
    cex.lab  = 1.5,
    level    = c(90, 95, 99),
    shade    = c("white", "gray55", "gray75"),
    refline  = 0
  )
  
  legend(
    "topright",
    inset  = c(0, 0.01),  # Adjust the legend position
    legend = sprintf("Egger's test (%s): z = %.02f, p = %.02f", var, eggers$zval, eggers$pval),
    bg     = "white"
  )
  
  dev.off()  # Close funnel plot device
  
  # -------------------------------
  # 3c. Generate Scatter Plots for Significant Moderators
  # -------------------------------
  
  # Check p-values from the meta-regression model for moderators
  idx      <- res_metaMOD$pval < 0.05
  label    <- c(NA, "Hospital beds", "Long-term care beds")  # Moderator labels
  mod_name <- c(NA, "hospitalbeds", "ltcbeds")              # Moderator variable names
  
  for (k in 2:3) {
    if (idx[k] == FALSE || k == 1) {
      next  # Skip if p-value is not significant or if k == 1
    } else {
      output_file <- file.path(wdir, "results",
                               sprintf("mods_regression.OR_%s.v1.0.png", var))
      width  <- 600
      height <- 400
      
      png(filename = output_file, width = width, height = height, units = "px", res = 96)
      
      regplot(
        res_metaMOD,
        xlim    = c(0, max(df_complete[[mod_name[k]]]) * 1.25),
        predlim = c(0, max(df_complete[[mod_name[k]]]) * 1.25),
        xlab    = label[k],
        ylab    = sprintf("Odds ratio for %s", var),
        refline = 0,
        atransf = exp,
        digits  = 1,
        las     = 1,
        bty     = "l",
        mod     = mod_name[k],
        at      = log(seq(0.6, 7.4, by = 1.2)),
        label   = c(1, 4, 15),
        labsize = 0.9,
        plim    = c(2, NA)
      )
      
      dev.off()  # Close scatter plot device
    }
  }
}

# -------------------------------
# 4. Correlation Test Between Moderators
# -------------------------------

ltcbds_vs_hosp <- cor.test(df_complete$ltcbeds, df_complete$hospitalbeds)

# Extract correlation coefficient and p-value
correlation     <- round(ltcbds_vs_hosp$estimate, 2)
p_value         <- signif(ltcbds_vs_hosp$p.value, 3)
annotation_text <- sprintf("r = %.2f, p = %.3f", correlation, p_value)

# Plot correlation between hospital and long-term care beds
output_file <- file.path(wdir, "results", sprintf("correlation_of_mods.v1.0.png"))
width  <- 900
height <- 800

png(filename = output_file, width = width, height = height, units = "px", res = 96)

plot(
  df_complete$ltcbeds, df_complete$hospitalbeds,
  main   = "Correlation between hospital and long-term beds",
  xlab   = "Long-term Care Beds [per 100,000 inhabitants]",
  ylab   = "Hospital Beds [per 100,000 inhabitants]",
  pch    = 19,
  frame  = FALSE,
  cex.main = 1.5,
  cex.lab  = 1.2,
  cex.axis = 1.2
)

abline(lm(hospitalbeds ~ ltcbeds, data = df_complete), col = "blue")

text(
  x      = max(df_complete$ltcbeds),
  y      = max(df_complete$hospitalbeds),
  labels = annotation_text,
  pos    = 2,      # Position annotation to the left
  cex    = 1.5,    # Increase text size
  col    = "black"
)

dev.off()  # Close correlation plot device

