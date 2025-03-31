# This is code to run meta-analyses on specific subitems of available data;
# For this, a list of specific commands is added such as the name, the 
# formula used for the regression, the title for the forest plot and the groups
# fpr which OR are estimated

# Code developed by Anna and David Pedrosa

# Version 1.1 # 2025-01-03, # Improving the documentation for this function

get_specific_information <- function(x) {
  if (x == "gender") {
    list_sheets <- list(
      name = 'gender',
      mods = '~ hospitalbeds + ltcbeds',
      title = 'Meta-analysis: odds that men die in hospital with respect to women',
      groups = c('Male', 'Female')
    )
  } else if (x == "city") {
    list_sheets <- list(
      name = 'city',
      mods = '~ hospitalbeds + ltcbeds', #mods = c(),
      title = 'Meta-analysis: odds that people from cities die in hospital',
      groups = c('City', 'Other')
    )
  } else if (x == "education") {
    list_sheets <- list(
      name = 'education',
      mods = '~ hospitalbeds + ltcbeds', #mods = c(),
      title = 'Meta-analysis: odds that people with higher education die in hospital',
      groups = c('Higher education', 'Lower education')
    )
  } else if (x == "marital_status") {
    list_sheets <- list(
      name = 'marital_status',
      mods = '~ hospitalbeds + ltcbeds', #mods = c(),
      title = 'Meta-analysis: odds that married people die in hospital',
      groups = c('Married', 'unmarried')
    )
  } else if (x == "age") {
    list_sheets <- list(
      name = 'age',
      mods = '~ hospitalbeds + ltcbeds', #mods = c(),
      title = 'Meta-analysis: odds that elderly persons die in hospital',
      groups = c('Elderly', 'younger')
    )
  } else if (x == "palliative_homecare") {
    list_sheets <- list(
      name = 'palliative_homecare',
      mods = c(),
      title = 'Meta-analysis: odds that the presence of palliative_homecare increase mortalities in hospitals',
      groups = c('Presence of palliative homecare', 'not')
    )
  } else   {
    stop("Invalid input")
  }
  
  return(list_sheets)
}
