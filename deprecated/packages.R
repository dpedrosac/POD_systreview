# Define a list of required packages for this project


packages = c(	"broom",
		"clubSandwich", 
		"dplyr",
		"lme4",
		"metafor", 
		"readxl", 
		"tidyverse")

# Check if the required packages are installed
# If not installed, install them and then load the library
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
