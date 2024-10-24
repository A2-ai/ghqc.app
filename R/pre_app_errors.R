rproj_root_dir <- function() {
  tryCatch(
    {
      root_dir <- rprojroot::find_rstudio_root_file()
      return(root_dir)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was no Rproj file found within the directory '{getwd()}'."))
      rlang::abort(e$message)
    }
  )
}


get_valid_checklists <- function() {
  tryCatch(
      {
        yaml_checklists <- get_checklists()
      },
      error = function(e) {
        error(.le$logger, glue::glue("There was an error retrieving checklists: {e$message}"))
        rlang::abort(e$message)
      }
  )

  return(yaml_checklists)
} # get_valid_checklists


get_org_errors <- function() {
  tryCatch(
    {
      get_organization()
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving organization: {e$message}"))
      rlang::abort(e$message)
    }
  )
}

get_repo_errors <- function(remote) {
  tryCatch(
    {
      get_current_repo(remote)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving repo: {e$message}"))
      rlang::abort(e$message)
    }
  )
}

get_members_errors <- function(org, repo) {
  tryCatch(
    {
      get_collaborators(owner = org, repo = repo)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving members: {e$message}"))
      rlang::abort(e$message)
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
      error(.le$logger, glue::glue("There was an error retrieving open Milestones: {e$message}"))
      rlang::abort(e$message)
    }
  )
}

get_all_milestone_list_errors <- function(org, repo) {
  tryCatch(
    {
      all_milestones <- list_milestones(org = org, repo = repo)
      rev(all_milestones)
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error retrieving all Milestones: {e$message}"))
      rlang::abort(e$message)
    }
  )
}

