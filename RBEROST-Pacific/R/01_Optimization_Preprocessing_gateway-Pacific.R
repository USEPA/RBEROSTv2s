##############################################################
### RBEROST Preprocessing Gateway                          ###
### Date: May 6, 2021                                      ###
### Author: Cathy Chamberlin                               ###
### Purpose: Bridge between RBEROST run file and R scripts ###
##############################################################

if(exists("IncludeUncertainty")) {
  if(IncludeUncertainty == TRUE) {
  source(
    "./RBEROST-Pacific/R/01_Optimization_Preprocessing+Uncertainty-Pacific.R"
    )
} else if(IncludeUncertainty == FALSE) {
    source("./RBEROST-Pacific/R/01_Optimization_Preprocessing-Pacific.R")
} else {
    print(
      "Only allowable options are 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'. You cannot have both, only one or the other."
    )
  }
} else {
  print(
    "Did you delete the line of code that says 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'?"
  )
  }