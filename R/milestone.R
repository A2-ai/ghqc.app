# milestone helper fns

# check if a milestone exists
#' @importFrom log4r warn error info debug
milestone_exists <- function(title) {
  # list milestones
  milestones <- get_all_milestone_objects()

  # return true if any matches
  any_matches <- any(sapply(milestones, function(milestone) milestone$title == title))
  return(any_matches)
}

#' @importFrom log4r warn error info debug
get_milestone_from_name <- function(name_in) {
  # list milestones
  milestones <- get_all_milestone_objects()
  # try to get milestone number
  milestone <- lapply(milestones, function(milestone) {
    if (milestone$title == name_in) {
      milestone
    }
    else {
      NULL
    }
  })
  # filter null values - return first match
  milestone <- Filter(Negate(is.null), milestone)

  # milestone is a list if there's more than 1 and a character value otherwise
  # for chr val: milestone[[1]] to give back api url but [[6]] is the ms number
  # for list: milestone[[1]] gives back the first milestone
  if (is.null(milestone) || length(milestone) == 0) {
    NULL
  }
  # else, if not null (milestone was found with that name)
  else {
      return(milestone[[1]])
  }
}

# look up number for milestone that exists - return null if it can't be found
#' @importFrom log4r warn error info debug
look_up_existing_milestone_number <- function(milestone_name) {
  debug(.le$logger, glue::glue("Retrieving Milestone: {milestone_name}"))
  milestone <- get_milestone_from_name(milestone_name)

  if (!is.null(milestone)) {
    if (!is.list(milestone)) {
      basename(milestone)
    } else {
      milestone$number
    }
  } else {
    debug(.le$logger, glue::glue("Milestone: {milestone_name} does not currently exist"))
    NULL
  }
}

#' @importFrom log4r warn error info debug
#' @importFrom log4r warn error info debug
create_milestone <- function(params) {
  params$.api_url <- .le$github_api_url

  debug(.le$logger, glue::glue("Creating Milestone: {params$title}..."))
  milestone <- do.call(gh::gh, c("POST /repos/:org/:repo/milestones", params))
  info(.le$logger, glue::glue("Created Milestone: {params$title}"))
  milestone
} # create_milestone

#' @importFrom log4r warn error info debug
get_milestone_number <- function(params) {

  searched_number <- tryCatch({
      look_up_existing_milestone_number(params$title)
    }, error = function(e){
      debug(.le$logger, glue::glue("No Milestones found: {conditionMessage(e)}"))
      return(NULL)
    })

  if (is.null(searched_number)){
    milestone <- create_milestone(params)
    milestone$number
  }
  else {
    debug(.le$logger, glue::glue("Retrieved Milestone: {params$title}, #{searched_number}"))
    searched_number
  }
} # get_milestone_number



