rproj_root_dir <- function() {
  tryCatch(
    {
      root_dir <- rprojroot::find_rstudio_root_file()

      if (getwd() != root_dir) {
        setwd(root_dir)
        info(.le$logger, glue::glue("Directory changed to project root: {root_dir}"))
      }

      return(root_dir)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was no Rproj file found within the directory '{getwd()}'."))
      rlang::abort(conditionMessage(e))
    }
  )
}


get_valid_checklists <- function() {
  tryCatch(
      {
        checklists <- get_checklists()
      },
      error = function(e) {
        error(.le$logger, glue::glue("There was an error retrieving {get_checklist_display_name_var(plural = TRUE)}: {conditionMessage(e)}"))
        rlang::abort(conditionMessage(e))
      }
  )

  return(checklists)
} # get_valid_checklists

get_members_errors <- function() {
  tryCatch(
    {
      get_collaborators()
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving members: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}



