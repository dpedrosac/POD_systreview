# This code provides the necessary subfunctions to run the main analysis (data_analysis.R)

# Code developed by Anna and David Pedrosa

## ========== ========== ========== ========== ##
# 1. Read MS Excel files
read_POD_Hospital_data <- function(sheet) {
  read_excel(
    path = file.path(data_dir, "datatables_POD_Hospital.xlsx"),
    sheet = sheet
  )
}

## ========== ========== ========== ========== ##
# 2. Add Q-test, I^2, and tau^2 estimate info (only if no moderators included).
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

## ========== ========== ========== ========== ##
# 3. Add Q-test, I^2, and tau^2 estimate info (if moderators included).
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
