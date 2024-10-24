untracked_changes <- function(qc_file) {
  status <- gert::git_status()
  if (qc_file %in% status$file) {
    TRUE
  }
  else FALSE

}

name_file_copy <- function(file_path) {
  dir_name <- dirname(file_path)
  file_name <- basename(file_path)
  file_extension <- tools::file_ext(file_name)
  file_base_name <- tools::file_path_sans_ext(file_name)

  file_copy_name <- paste0(file_base_name, "_copy_for_ghqc.", file_extension)
  file.path(dir_name, file_copy_name)
}

rename_file_copy <- function(file_path) {
  file.rename(file_path, stringr::str_remove(file_path, "_copy_for_ghqc"))
}

#' @importFrom log4r warn error info debug
read_file_at_commit <- function(commit_sha, file_path) {
  args <- c("checkout", commit_sha, "--", file_path)
  result <- processx::run("git", args, error_on_status = FALSE)

  if (result$status != 0) {
    stop(result$stderr)
  }

  #tryCatch({
    file_content <- suppressWarnings(readLines(file_path))
  # }, error = function(e) {
  #   if (stringr::str_detect(e$message, "incomplete final line")) {
  #     # add a newline
  #     con <- file(file_path, open = "a")
  #     writeLines("\n", con)
  #     close(con)
  #
  #     # read file
  #     file_content <- suppressWarnings(readLines(file_path))
  #
  #     # remove newline
  #     if (length(lines) > 0 && lines[length(lines)] == "") {
  #       lines <- lines[-length(lines)]
  #     }
  #     writeLines(lines, file_path)
  #   } # if incomplete final line
  # })

  return(file_content)
}

extract_line_numbers <- function(text) {
  match <- stringr::str_match(text, "@@ ([^@]+) @@")[2]
  first_set <- stringr::str_match(match, "^\\s*(\\d+)(?:,(\\d+))?")[,2:3]
  second_set <- stringr::str_match(match, "/\\s*(\\d+)(?:,(\\d+))?\\s*$")[,2:3]
  list(reference = as.numeric(first_set), comparator = as.numeric(second_set))
}

format_line_numbers <- function(numbers) {
  # if there's just one line, it prints like
  # ("@@ 1 / 1,5 @@"
  # instead of
  # ("@@ 1,1 / 1,5 @@"
  # to not be verbose
  # so this fixes it to be verbose for parsing ease
  if (is.na(numbers$reference[2])) {numbers$reference[2] <- 1}
  if (is.na(numbers$comparator[2])) {numbers$comparator[2] <- 1}

  reference <- glue::glue("{numbers$reference[1]}-{numbers$reference[1]+numbers$reference[2]-1}")
  comparator <- glue::glue("{numbers$comparator[1]}-{numbers$comparator[1]+numbers$comparator[2]-1}")

  glue::glue("@@ - previous: lines {reference} @@\n@@ + current:  lines {comparator} @@")
}

add_line_numbers <- function(text) {
  # get start and end lines for ref and comp
  reference_lines <- stringr::str_match(text, "@@ \\- previous: lines (\\d+)-(\\d+) @@")[,2:3]
  comparator_lines <- stringr::str_match(text,"@@ \\+ current:  lines (\\d+)-(\\d+) @@")[,2:3]

  reference_lines_start <- as.numeric(reference_lines[1])
  comparator_lines_start <- as.numeric(comparator_lines[1])

  # get lines from text
  lines <- stringr::str_split(text, "\n")[[1]]

  # increment on ref and comp lines
  ref_line_num <- reference_lines_start
  comp_line_num <- comparator_lines_start

  new_lines <- sapply(lines, function(line) {
    if (stringr::str_detect(line, "^- ")) {
      # prev script line
      new_line <- stringr::str_replace(line, "^- ", glue::glue("- {ref_line_num} "))
      ref_line_num <<- ref_line_num + 1
    } else if (stringr::str_detect(line, "^\\+ ")) {
      # current script line
      new_line <- stringr::str_replace(line, "^\\+ ", glue::glue("+ {comp_line_num} "))
      comp_line_num <<- comp_line_num + 1
    } else if (stringr::str_detect(line, "^  ")) {
      # unmodified line
      new_line <- stringr::str_replace(line, "^  ", glue::glue("  {comp_line_num} "))
      comp_line_num <<- comp_line_num + 1
      ref_line_num <<- ref_line_num + 1
    } else {
      # empty line
      new_line <- line
    }
    new_line
  })

  glue::glue_collapse(new_lines, sep = "\n")
}

clean_up <- function(file_path, copied_file) {
  # delete copy at previous commits
  fs::file_delete(file_path)
  # rename file to original name
  rename_file_copy(copied_file)

  # finally, just read the file at the most recent commit to avoid
  # having untracked changes
  most_recent_commit <- gert::git_log(max = 1)$commit
  read_file_at_commit(most_recent_commit, file_path)
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

get_script_contents <- function(file_path, reference, comparator) {
  # create copy
  copied_file <- name_file_copy(file_path)
  file.copy(file_path, copied_file)
  withr::defer_parent(
    clean_up(file_path, copied_file)
  )

  # get file contents at the specified commits
  reference_script <- read_file_at_commit(reference, file_path)
  comparator_script <- read_file_at_commit(comparator, file_path)

  list(reference_script = reference_script, comparator_script = comparator_script)
}

format_diff <- function(reference_script, comparator_script) {
  # get diff
  diff_output <- diffobj::diffChr(reference_script, comparator_script, format = "raw", mode = "unified", pager = "off", disp.width = 200)
  diff_lines <- as.character(diff_output)

  # get the line indices with the file names (either 1,2 or 2,3 depending on if the the files were the same)
  if (diff_lines[1] == "No visible differences between objects.") {
    #2
    return("\nNo difference between file versions.\n")
  }

  # delete the lines with the file names
  diff_lines <- diff_lines[-c(1, 2)]

  # lines that start with @@
  section_indices <- grep("^@@", diff_lines)

  # add the end index to the section indices
  section_indices <- c(section_indices, length(diff_lines) + 1)

  # split into sections
  sections <- lapply(1:(length(section_indices) - 1), function(i) {
    start_idx <- section_indices[i]
    end_idx <- section_indices[i + 1] - 1
    paste(diff_lines[start_idx:end_idx], collapse = "\n")
  })

  # apply diff to each section
  diff_sections <- lapply(sections, format_diff_section)

  # combine sections to one body of text
  diff_sections_cat <- glue::glue_collapse(diff_sections, sep = "\n")

  glue::glue("```diff\n{diff_sections_cat}\n```")
}

get_comments <- function(owner, repo, issue_number) {
  comments <- gh::gh(
    "GET /repos/:owner/:repo/issues/:issue_number/comments", .api_url = .le$github_api_url,
    owner = owner,
    repo = repo,
    issue_number = issue_number
  )
  comments_df <- do.call(rbind, lapply(comments, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)))
}

# returns true if the user can check "compare to most recent qc fix"
# false otherwise
# check_if_there_are_update_comments <- function(owner, repo, issue_number) {
#   comments <- get_comments(owner, repo, issue_number)
#   if (length(comments) == 0) return(FALSE)
#   most_recent_qc_commit <- get_commit_from_most_recent_update_comment(comments)
#   if (is.na(most_recent_qc_commit)) return(FALSE)
#   else return(TRUE)
# }

# gets the most recent qc update commit from the comments in the issue
# if there are no update comments from the author, it returns NA
# get_commit_from_most_recent_update_comment <- function(comments_df) {
#   # sort by descending creation time
#   comments_df <- comments_df %>% dplyr::arrange(dplyr::desc(created_at))
#
#   # loop through comments, grab the first one
#   for (i in seq_len(nrow(comments_df))) {
#     comment <- comments_df[i, ]
#     commit_from_comment <- get_current_commit_from_comment(comment$body)
#     if (!is.na(commit_from_comment)) {
#       return(commit_from_comment)
#     }
#   }
#
#   return(NA)
# }

# get_current_commit_from_comment <- function(body) {
#   stringr::str_match(body, "\\* current QC request commit: ([a-f0-9]+)")[,2]
# }
