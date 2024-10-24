untracked_changes <- function(qc_file) {
  status <- gert::git_status()
  if (qc_file %in% status$file) {
    TRUE
  }
  else FALSE

}

extract_line_numbers <- function(text) {
  match <- stringr::str_match(text, "@@ ([^@]+) @@")[2]
  first_set <- stringr::str_match(match, "^\\s*(\\d+)(?:,(\\d+))?")[,2:3]
  second_set <- stringr::str_match(match, "/\\s*(\\d+)(?:,(\\d+))?\\s*$")[,2:3]
  list(previous = as.numeric(first_set), current = as.numeric(second_set))
}

format_line_numbers <- function(numbers) {
  # if there's just one line, it prints like
  # ("@@ 1 / 1,5 @@"
  # instead of
  # ("@@ 1,1 / 1,5 @@"
  # to not be verbose
  # so this fixes it to be verbose for parsing ease
  if (is.na(numbers$previous[2])) {numbers$previous[2] <- 1}
  if (is.na(numbers$current[2])) {numbers$current[2] <- 1}

  previous <- glue::glue("{numbers$previous[1]}-{numbers$previous[1]+numbers$previous[2]-1}")
  current <- glue::glue("{numbers$current[1]}-{numbers$current[1]+numbers$current[2]-1}")

  glue::glue("@@ previous script: lines {previous} @@\n@@  current script: lines {current} @@")
}

add_line_numbers <- function(text) {
  # get start and end lines for prev and current scripts
  prev_lines <- stringr::str_match(text, "@@ previous script: lines (\\d+)-(\\d+) @@")[,2:3]
  current_lines <- stringr::str_match(text, "@@  current script: lines (\\d+)-(\\d+) @@")[,2:3]

  prev_start <- as.numeric(prev_lines[1])
  current_start <- as.numeric(current_lines[1])

  # get lines from text
  lines <- stringr::str_split(text, "\n")[[1]]

  # increment on prev and current lines
  prev_line_num <- prev_start
  current_line_num <- current_start

  new_lines <- sapply(lines, function(line) {
    if (stringr::str_detect(line, "^- ")) {
      # prev script line
      new_line <- stringr::str_replace(line, "^- ", glue::glue("- {prev_line_num} "))
      prev_line_num <<- prev_line_num + 1
    } else if (stringr::str_detect(line, "^\\+ ")) {
      # current script line
      new_line <- stringr::str_replace(line, "^\\+ ", glue::glue("+ {current_line_num} "))
      current_line_num <<- current_line_num + 1
    } else if (stringr::str_detect(line, "^  ")) {
      # unmodified line
      new_line <- stringr::str_replace(line, "^  ", glue::glue("  {current_line_num} "))
      current_line_num <<- current_line_num + 1
      prev_line_num <<- prev_line_num + 1
    } else {
      # empty line
      new_line <- line
    }
    new_line
  })

  glue::glue_collapse(new_lines, sep = "\n")
}

format_diff_section <- function(diff_lines) {
  diff_lines <- strsplit(diff_lines, "\n")[[1]]
  # extract the line numbers
  numbers <- extract_line_numbers(diff_lines[1])
  # reformat line numbers
  context_str <- format_line_numbers(numbers)
  # replace with new context_str
  diff_lines[1] <- context_str

  # check if last line is tick marks for formatting
  if (stringr::str_detect(diff_lines[length(diff_lines)], "```")) {
    diff_lines <- diff_lines[-c(length(diff_lines))]
  }

  format_diff_for_github <- function(diff_lines) {
    result <- c()
    for (line in diff_lines) {
      if (startsWith(line, ">")) {
        result <- c(result, paste0("+", substr(line, 2, nchar(line))))
      } else if (startsWith(line, "<")) {
        result <- c(result, paste0("-", substr(line, 2, nchar(line))))
      } else {
        result <- c(result, paste0(line))
      }
    }
    return(result)
  }

  github_diff <- format_diff_for_github(diff_lines)

  diff_cat <- glue::glue_collapse(github_diff, sep = "\n")

  diff_with_line_numbers <- add_line_numbers(diff_cat)
}


get_comments <- function(owner, repo, issue_number) {
  comments <- gh::gh(
    "GET /repos/:owner/:repo/issues/:issue_number/comments",
    .api_url = .le$github_api_url,
    owner = owner,
    repo = repo,
    issue_number = issue_number
  )
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}
