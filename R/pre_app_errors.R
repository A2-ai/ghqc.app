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
        yaml_checklists <- get_checklists()
      },
      error = function(e) {
        error(.le$logger, glue::glue("There was an error retrieving {get_checklist_display_name_var(plural = TRUE)}: {conditionMessage(e)}"))
        rlang::abort(conditionMessage(e))
      }
  )

  return(yaml_checklists)
} # get_valid_checklists


get_org_errors <- function(remote) {
  tryCatch(
    {
      get_organization(remote)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving organization: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}

get_repo_errors <- function(remote) {
  tryCatch(
    {
      get_current_repo(remote)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving repo: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}

get_members_errors <- function(org, repo) {
  tryCatch(
    {
      get_collaborators(owner = org, repo = repo)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving members: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}

get_open_milestone_list_errors <- function(org, repo) {
  tryCatch(
    {
      milestone_list <- get_open_milestone_names(org = org, repo = repo)
      rev(milestone_list)
    },
    error = function(e) {
      # it's fine to swallow error for this because milestones are not needed for creating
      error(.le$logger, glue::glue("There was an error retrieving open Milestones: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}

get_all_milestone_list_errors <- function(org, repo) {
  tryCatch(
    {
      all_milestones <- list_ghqc_milestone_names(org = org, repo = repo)
      rev(all_milestones)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving all Milestones: {conditionMessage(e)}"))
      rlang::abort(conditionMessage(e))
    }
  )
}

