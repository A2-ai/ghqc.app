approve_warnings <- function(row) {
  browser()
  warnings <- c()
  # 1: Is the user the QCer?
  user <- get_user()
  qcer <- row$QCer

  if (user != qcer) {
    warning <- "It is highly recommended that the QCer approves the final state of the QC file."
    warnings <- c(warnings, warning)
  }
  if (qcer == "No QCer") {
    warning <- "There is no QCer assigned for the Issue. If you are the QCer, assign yourself on GitHub before proceeding."
    warnings <- c(warnings, warning)
  }

  # 2: Are there QC comments in-line in the script still?

  # 3: Are there unchecked checklist items?


  return(warnings)
}
