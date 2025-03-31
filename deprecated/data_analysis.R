# This is code to run meta-analyses in dtaa available on place of death for people suffering from
# Parkinson's disease; 
# Code developed by Anna and David Pedrosa

# Version 1.3 # (2025-07-01) - added functionality, some more generic filenames and fixed indentation
# Version 1.2 (2024-11-25) - Tidying up the preliminary code

# Load required packages and directories
source("packages.R")         		# Load required packages
source("subfunctions_used.R") 		# Functions: 1. Read Excel data, 2. Estimate I^2, etc.
source("sheet_information.R") 		# Read sheet information as per function input

username <- Sys.getenv("USER") # Set working directory (wdir) based on the username
if (username == "dpedr") {
  wdir <- "D:/POD_systReview"
  data_dir <- file.path(wdir, "data")
} else if (username == "dpedrosac") {
  wdir <- "/media/storage/POD_systReview"
  data_dir <- file.path(wdir, "data")
} else {
  cat("Username unknown\n")
}
setwd(wdir)

# Flag for performing additional checks if necessary
# flag_check <- FALSE # not needed here

# Sheets to analyze
sheets_to_analyse <- c("gender", "city", "education", "marital_status", "age") # note: "palliative_homecare" not working so far

# Initialize lists to store meta-analysis results
res_metaMOD_list <- list()
res_meta_list <- list()


for (var in sheets_to_analyse) {
  
  # Retrieve sheet-specific information
  list_sheets <- get_specific_information(var)
  
  # Import and clean data
  df <- read_POD_Hospital_data(list_sheets$name) %>%
    mutate(
      across(-2, ~ as.numeric(as.character(.))), # Converts "p < .001" to NA
      se = sqrt(1 / t_pos + 1 / t_neg + 1 / c_pos + 1 / c_neg)
    )
  
  df_complete <- df %>%
    filter(complete.cases(t_pos, t_neg, c_pos, c_neg)) %>%
    mutate(
      fisher_result = pmap(list(t_pos, t_neg, c_pos, c_neg), function(tp, tn, cp, cn) {
        fisher.test(matrix(c(tp, tn, cp, cn), nrow = 2))
      }),
      p_values = map_dbl(fisher_result, ~ tidy(.x)$p.value),
      same_publication = if_else(str_detect(author, "Moens"), 1, 0)
    ) %>%
    select(-fisher_result)

  df_complete$number <- seq_len(nrow(df_complete))
  
# Perform meta-analysis
  df_complete <- conv.wald(out = OR, ci.lb = ci.lb, ci.ub = ci.ub, pval = p_values, n = n_ges, 
                           data = df_complete, transf = log)
  
  # Common parameters for meta-analysis
  common_params <- list(
    yi = df_complete$yi,
    vi = df_complete$vi,
    random = ~ 1 | as.factor(df_complete$same_publication) / number,
    verbose = TRUE,
    control = list(optimizer = "optim", optmethod = "Nelder-Mead"),
    data = df_complete
  )
  
  # Meta-analysis with or without moderators
  if (!is.null(list_sheets$mods)) {
    res_metaMOD <- do.call(rma, c(common_params, list(mods = as.formula(list_sheets$mods))))
    ylim2 <- -2.5
  } else {
    res_metaMOD <- do.call(rma, common_params)
    ylim2 <- -1.5
  }
  
# Generate forest plot
  output_file <- file.path(wdir, "results", sprintf("fig%.2d.forest_plot.OR_%s.v1.0.png", 
                                                     which(sheets_to_analyse == var), var))
  png(filename = output_file, width = 1800, height = 1250, units = "px", res = 96)

  forest1 <- forest(
    res_metaMOD, 
    xlim = c(-11, 6), ylim = c(ylim2, res_metaMOD$k + 3), atransf = exp, showweights = TRUE,
    addfit = TRUE, addpred = TRUE,
    # order = "ltcbeds", 
    xlab = "Odds ratio [OR]",
    slab = NA,
    mlab = mlabfun("RE Model for all Studies", res_metaMOD),
    ilab = cbind(df_complete$author, df_complete$year, df_complete$t_pos, df_complete$t_neg, 
                 df_complete$c_pos, df_complete$c_neg, df_complete$n_ges),
    ilab.pos = 4,
    ilab.xpos = c(-11, -8, -7, -6, -5, -4, -3),
    alim = c(-4, 3), at = log(c(0.25, 1, 4, 20)),
    cex = 1.25, main = list_sheets$title, shade = TRUE
  )
  
  segments(-7, forest1$ylim[2]-.5, -5.5, forest1$ylim[2]-.5)
  segments(-5, forest1$ylim[2]-.5, -3.5, forest1$ylim[2]-.5)

  # Additional plot text
  text(c(-6.25, -4.25), res_metaMOD$k + 3, font = 2, cex = cex_plot, c(list_sheets$groups[1], list_sheets$groups[2]))
  text(
    c(-10.5, -7.75, -6.75, -5.75, -4.75, -3.75, -2.75, 4.5),
    res_metaMOD$k + 2, cex = cex_plot,
    c("Author(s)", "year", "hosp.", "other", "hosp.", "other", expression("n"[tot]), "Weights and OR [95%-CI]")
  )

  res_meta <- rma( # meta analysis without moderators
    yi, vi, random = ~ 1 | as.factor(df_complete$same_publication) / number,
    verbose = TRUE, control = list(optimizer = "optim", optmethod = "Nelder-Mead"),
    data = df_complete
  )

  # Add res_metaMOD result to the list
  res_metaMOD_list[[var]] <- res_metaMOD
  res_meta_list[[var]] <- res_meta

  if (!is.null(list_sheets$mods)) {
	# Predictions
	sav1 <- predict(res_meta)
	addpoly(res_meta, row=-1, mlab=mlabfun("Random-Effects Model", res_meta))

	sav2 <- predict(res_metaMOD, newmods = c(mean(df_complete$hospitalbeds), mean(df_complete$ltcbeds))) # use average number for both continuous moderators
	addpoly(sav2, row = -2, mlab = "Meta-Regression Model (Adjusted Effect)") # see: https://www.metafor-project.org/doku.php/tips:computing_adjusted_effects
	# addpoly(res_metaMOD, row=-3, mlab=mlabfun_MOD("Meta-Regression Model (Adjusted Effect)", res_metaMOD))
  }

  abline(h = 0)

  # Close the graphics device
  dev.off()

  # For help with interpretation of results see here: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/metareg.html

  # Print confirmation message
  cat("Forest plot saved to", output_file, "\n")

  # Start plotting funnel plots

  # Specify file name and dimensions for the funnel plot
  output_file <- file.path(wdir, "results", 
                         sprintf("suppl.material.fig%.2d.eggers_%s.v1.0.png", 
                                 which(sheets_to_analyse == var), var))
  width <- 600
  height <- 800

  # Open a new graphics device with the specified dimensions
  png(filename = output_file, width = width, height = height, units = "px", res = 96)

  # Perform Egger's test for asymmetry
  eggers <- regtest(res_metaMOD)

  # Plot the funnel plot
  funnel(
    res_metaMOD, 
    cex = 2, cex.axis = 1.5, cex.lab = 1.5,
    level = c(90, 95, 99), 
    shade = c("white", "gray55", "gray75"), 
    refline = 0
  )

  # Add a legend with the results of Egger's test
  legend(
    "topright",
    inset = c(0, 0.01),  # Adjust the position of the legend
    legend = sprintf("Egger's test (%s): z = %.02f, p = %.02f", var, eggers$zval, eggers$pval),
    bg = "white" 
    )

  # Close the graphics device
  dev.off()

  # Check p-values from the meta-regression model
  idx <- res_metaMOD$pval < .05
  label <- c(NA, "Hospital beds", "Long-term care beds")  # Labels for moderators
  mod_name <- c(NA, "hospitalbeds", "ltcbeds")  # Labels for moderators

  # Generate scatter plots for significant moderators
  for (k in 2:3) {
    if (idx[k] == FALSE || k == 1) { 
      next  # Skip if p-value is not significant or k == 1
    } else {
    # Specify file name and dimensions for the scatter plot
      output_file <- file.path(wdir, "results", 
                             sprintf("mods_regression.OR_%s.v1.0.png", var))
      width <- 600
      height <- 400

      # Open a new graphics device with the specified dimensions
      png(filename = output_file, width = width, height = height, units = "px", res = 96)

      # Create a scatter plot for the moderator
      regplot(
        res_metaMOD, 
        xlim = c(0, max(df_complete[[mod_name[k]]])*1.25), predlim = c(0, max(df_complete[[mod_name[k]]])*1.25), 
        xlab = label[k], 
        ylab = sprintf("Odds ratio for %s", var), 
        refline = 0,
        atransf = exp, 
        digits = 1, 
        las = 1, 
        bty = "l", 
        mod = mod_name[k], 
        at = log(seq(0.6, 7.4, by = 1.2)),
        label = c(1, 4, 15), 
        labsize = 0.9, 
        plim = c(2, NA)
      )

      # Close the graphics device
      dev.off()
    }
  }
}

# Correlation test
ltcbds_vs_hosp <- cor.test(df_complete$ltcbeds, df_complete$hospitalbeds)

# Extract correlation coefficient and p-value
correlation <- round(ltcbds_vs_hosp$estimate, 2)
p_value <- signif(ltcbds_vs_hosp$p.value, 3)
annotation_text <- sprintf("r = %.2f, p = %.3f", correlation, p_value)

# Specify file name and dimensions
output_file <- file.path(wdir, "results", sprintf("correlation_of_mods.v1.0.png"))
width <- 900
height <- 800

# Open a new graphics device with the specified dimensions
png(filename = output_file, width = width, height = height, units = "px", res = 96)

# Plot with main and axis titles
plot(df_complete$ltcbeds, df_complete$hospitalbeds, 
     main = "Correlation between hospital and long-term beds",
     xlab = "Long-term Care Beds [per 100,000 inhabitants]", 
     ylab = "Hospital Beds [per 100,000 inhabitants]",
     pch = 19, frame = FALSE, cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)

# Add regression line
abline(lm(hospitalbeds ~ ltcbeds, data = df_complete), col = "blue")

# Annotate the correlation results
text(
  x = max(df_complete$ltcbeds), 
  y = max(df_complete$hospitalbeds), 
  labels = annotation_text, 
  pos = 2, # Position annotation to the left
  cex = 1.5, # Increase text size
  col = "black"
)

dev.off()
