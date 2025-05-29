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

get_script_contents <- function(reference_file_path, comparator_file_path, reference_commit, comparator_commit) {
  temp_dir <- tempdir()
  file_diff_dir <- file.path(temp_dir, "file_diff_dir")
  fs::dir_create(file_diff_dir)
  withr::defer({
    if (dir.exists(file_diff_dir)) {
      fs::dir_delete(file_diff_dir)
    }
  })

  # name the files the contents will be redirected to
  file_at_reference <- tempfile(tmpdir = file_diff_dir)
  file_at_comparator <- tempfile(tmpdir = file_diff_dir)

  # get reference file contents
  command_ref <- glue::glue("git show {reference_commit}:\"{reference_file_path}\" > {file_at_reference}")
  result_ref <- processx::run("sh", c("-c", command_ref), error_on_status = FALSE)

  if (result_ref$status != 0) {
    rlang::abort(message = glue::glue(
    "status: {result_ref$status}
    stdout: {result_ref$stdout}
    stderr: {result_ref$stderr}
    timeout: {result_ref$timeout}")
    )
  }

  # get reference file contents
  command_comp <- glue::glue("git show {comparator_commit}:\"{comparator_file_path}\" > {file_at_comparator}")
  result_comp <- processx::run("sh", c("-c", command_comp), error_on_status = FALSE)

  if (result_comp$status != 0) {
    rlang::abort(message = glue::glue(
      "status: {result_comp$status}
      stdout: {result_comp$stdout}
      stderr: {result_comp$stderr}
      timeout: {result_comp$timeout}")
    )
  }

  # read file contents
  reference_script <- suppressWarnings(readLines(file_at_reference))
  comparator_script <- suppressWarnings(readLines(file_at_comparator))

  list(reference_script = reference_script,
       comparator_script = comparator_script
       )
}

format_diff <- function(reference_script, comparator_script) {
  # get diff
  diff_output <- diffobj::diffChr(reference_script, comparator_script, format = "raw", mode = "unified", pager = "off", disp.width = 200)
  diff_lines <- as.character(diff_output)

  # get the line indices with the file names (either 1,2 or 2,3 depending on if the the files were the same)
  if (diff_lines[1] == "No visible differences between objects.") {
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

