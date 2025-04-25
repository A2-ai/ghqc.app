create_qc_data_section <- function(issue_creation_time, issue_creator, issue_title, issue_number, milestone_title, milestones) {
  sections <- c()
  # get qc_initializer
  humanized_creation_time <- humanize_time(issue_creation_time)
  qc_initializer <- glue::glue("{issue_creator} at {humanized_creation_time}")

  # issue number and qc_init sections
  qc_init_section <- glue::glue("* **QC initializer:** {qc_initializer}")
  issue_number_section <- glue::glue("* **Issue number:** {issue_number}")
  milestone_section <- create_milestone_section(milestone_title, milestones)
  sections <- c(sections, qc_init_section, issue_number_section, milestone_section)

  # create body
  issue_body <- glue::glue_collapse(sections, sep = "\n\n")
  issue_section <- create_small_section("QC Data", issue_body)
}

create_milestone_section <- function(milestone_title, milestones) {
  if (!is.null(milestone_title)) {
    milestone_body <- {
      description <- get_milestone_description(milestone_title, milestones)
      if (!is.null(description) && description != "") {
        glue::glue("* **Milestone:** {milestone_title}
                   * **Milestone description:** {description}")
      }
      else {
        glue::glue("* **Milestone:** {milestone_title}")
      }
    }
  }
  else ""
}

create_assignees_section <- function(assignees) {
  assignees_list <- sapply(assignees, function(assignee) glue::glue("- {assignee$login}"))
  assignees_body <- glue::glue_collapse(assignees_list, sep = "\n\n")
  assignees_section <- create_small_section("Assigned QCers", assignees_body)
}
create_comments_section <- function(issue_comments) {
  comments_list <- process_comments(issue_comments)
  comments_body <- glue::glue_collapse(comments_list, sep = "\n\n")
  clean_comments_body <- clean_comment_body(comments_body)
  comments_section <- create_small_section("Comments", clean_comments_body)
}

create_events_section <- function(events_list) {
  events_body <- glue::glue_collapse(events_list, sep = "\n")
  events_section <- create_small_section("Events", events_body)
}

create_status_section <- function(events_list, issue_state) {
  # status
  closures <- events_list[grep("closed", events_list)]

  status <- {
    if (length(closures) == 0) {issue_state} # should be open (in theory)
    else {
      # get the last time it was closed, which is the current status
      last_closure <- closures[length(closures)]
      gsub("- ", "", last_closure)
    }
  }
  status_section <- create_small_section("Issue Status", status)
}

create_timeline_section <- function(timeline) {
  timeline_list <- get_timeline_list(timeline)
  timeline_body <- glue::glue_collapse(timeline_list, sep = "\n")
  timeline_section <- create_small_section("Detailed Timeline", timeline_body)
}

clean_body <- function(body) {
  body <- stringr::str_replace_all(body, "(^#{1,4} .*|\\n#{1,4} .*)", function(x) {
    line <- stringr::str_extract(x, "(?<=#{1,4} ).*")
    line <- stringr::str_trim(line)
    paste0("\n", "**", line, "**")
  })
  return(body)
}

clean_comment_body <- function(body) {
  # cannot be more general here because "### Title" might exist in the code
  stringr::str_replace_all(body, "(## )(File Difference|Metadata)", function(x) {
    extracted <- stringr::str_remove(x, "## ")
    new <- paste0("**", extracted, "**\n\n")
    new
  })
}

create_checklist_section <- function(issue_body) {
  clean_issue_body <- clean_body(issue_body)
  checklist_section <- create_small_section("Issue Body", clean_issue_body)
}

issue_to_markdown <- function(issue_number) {
  # collect issue info
  issue <- get_issue(issue_number)
  milestones <- get_all_milestone_objects()

  issue_comments <- get_issue_comments(issue_number)

  issue_events <- get_issue_events(issue_number)
  events_list <- get_events_list(issue_events)

  timeline <- get_issue_timeline(issue_number)

  # create sections
  issue_section <- create_qc_data_section(issue_creation_time = issue$created_at,
                                          issue_creator = issue$user$login,
                                          issue_title = issue$title,
                                          issue_number = issue$number,
                                          milestone_title = issue$milestone$title,
                                          milestones = milestones)

  assignees_section <- create_assignees_section(issue$assignees)

  status_section <- create_status_section(events_list, issue$state)

  checklist_section <- create_checklist_section(issue$body)

  comments_section <- create_comments_section(issue_comments)

  events_section <- create_events_section(events_list)

  timeline_section <- create_timeline_section(timeline)

  # put it all together
  paste0(
    issue_section,
    assignees_section,
    status_section,
    checklist_section,
    comments_section,
    events_section,
    timeline_section
  )
} # issue_to_markdown

get_pdf_name <- function(input_name, milestone_names, just_tables, repo) {
  milestone_str <- glue::glue_collapse(milestone_names, "-")

  base_name <- {
    if (is.null(input_name) || input_name == "") {
      if (just_tables) {
        glue::glue("tables-{.le$repo}-{milestone_str}")
      }
      else {
        glue::glue("{.le$repo}-{milestone_str}")
      }

    }
    else { # remove .pdf if at the end
      stringr::str_remove(input_name, "\\.pdf$")
    }
  }

  # cleaning up:

  # check num chars
  if (nchar(base_name) > 60) {
    base_name <- substr(base_name, 1, 60)
  }

  # replace spaces and _ with -
  clean_name <- stringr::str_replace_all(base_name, "[ _]", "-")

  # remove special characters except for dashes and numbers
  clean_name <- stringr::str_remove_all(clean_name, "[^0-9A-Za-z\\-]")

  # make lowercase
  clean_name <- tolower(clean_name)

  pdf_name <- glue::glue("{clean_name}.pdf")
  return(pdf_name)
}

#' @importFrom log4r warn error info debug
markdown_to_pdf <- function(rmd_content, repo, milestone_names, just_tables, location, pdf_name) {
  debug(.le$logger, "Creating QC Record pdf...")
  # create temporary rmd
  rmd <- tempfile(fileext = ".Rmd")

  fs::file_create(rmd)
  info(.le$logger, glue::glue("Rmd location: {rmd}"))

  # for parsing rmds, need this so quarto setup global options chunk works
  rmd_content <- stringr::str_replace_all(rmd_content, "```diff", "```{diff}")

  writeLines(rmd_content, con = rmd)

  # create pdf from rmd
  location <- normalizePath(location)
  tryCatch({
    suppressWarnings(
      output_file <- rmarkdown::render(
        input = rmd,
        output_format = "pdf_document",
        output_file = pdf_name,
        output_dir = location,
        run_pandoc = TRUE,
        quiet = TRUE
      )
    )

    pdf_path_abs <- get_simple_path(output_file)

    info(.le$logger, "Converted rmd to pdf")
    info(.le$logger, glue::glue("Created QC Record pdf: {pdf_path_abs}"))

    return(pdf_path_abs)
  }, error = function(e) {

    # if error, put rmd and other files in project dir
    # create a folder to copy QC Record files into
    error_folder <- fs::dir_create(file.path(location, tools::file_path_sans_ext(basename(rmd))))

    # parse rmd contents for sourced files
    temp_dir_escaped <- stringr::str_replace_all(tempdir(), "/", "\\\\/")
    pattern <- paste0(temp_dir_escaped, "[^\"\\)\\]\\s]+")
    sourced_files <- unlist(stringr::str_extract_all(rmd_content, pattern))

    # copy rmd and sourced files to folder in directory
    files_to_copy <- c(rmd, sourced_files)
    file.copy(files_to_copy, error_folder, overwrite = TRUE)

    # replace sourced file paths in rmd with paths to copied files in user directory
    modified_rmd_content <- stringr::str_remove_all(rmd_content, paste0(tempdir(), "/"))
    writeLines(modified_rmd_content, file.path(error_folder, basename(rmd)))

    # print error message
    simple_error_folder_path <- get_simple_path(error_folder)
    rlang::abort(glue::glue("Error generating pdf. Please review generated Rmd and sourced files located in {simple_error_folder_path}"))
  })

} # markdown_to_pdf

get_summary_table_col_vals <- function(issue) {
  metadata <- {
    tryCatch({
      get_issue_body_metadata(issue$body)
    }, error = function(e) {
      # rename file path to issue title if not a ghqc issue

      tryCatch({
        list(
          `qc type` = "NA"
        )
      }, error = function(e) {
        rlang::abort(glue::glue("Issue: \"{issue$title}\" in Milestone: \"{issue$milestone$title}\" has a metadata section that cannot be parsed."))
      })

    })
  }

  if(length(metadata) == 0) {
    rlang::abort(glue::glue("Issue: \"{issue$title}\" in Milestone: \"{issue$milestone$title}\" was not created with ghqc and therefore cannot be parsed."))
  }

  close_data <- get_close_info(issue)

  authors <- get_authors(issue$title)
  latest_author <- authors$latest

  file_path <- issue$title
  author <- ifelse(!is.null(metadata$author), metadata$author, "NA")
  qcer <- ifelse(length(issue$assignees) > 0, issue$assignees[[1]], "NA")
  issue_closer <- ifelse(!is.null(close_data$closer), close_data$closer, "NA")
  close_date <- ifelse(!is.null(close_data$closed_at), close_data$closed_at, "NA")

  c(
    file_path = file_path,
    author = author,
    qcer = qcer,
    issue_closer = issue_closer,
    close_date = close_date
  )
}

get_summary_df <- function(issues) {
  col_vals <- lapply(issues, get_summary_table_col_vals)
  list_of_vectors <- lapply(col_vals, function(vec) {
    as.data.frame(as.list(vec))
  })

  df <- dplyr::bind_rows(list_of_vectors)
}


create_big_section <- function(section_title, contents) {
  glue::glue("# {section_title}\n\n{contents}\n\n\\newpage\n\n", .trim = FALSE)
} # create_section

create_medium_section <- function(section_title, contents) {
  glue::glue(
    "## {section_title}\n\n{contents}\n\n\\newpage\n\n", .trim = FALSE)
} # create_section

create_small_section <- function(section_title, contents) {
  glue::glue(
    "### {section_title}\n\n{contents}\n\n", .trim = FALSE)
} # create_section

insert_breaks <- function(text, width) {
  sapply(text, function(x) {
    words <- strsplit(x, "\\s+")[[1]]
    broken_words <- sapply(words, function(word) {
      if (nchar(word) > width) {
        paste(strsplit(word, paste0("(?<=.{", width, "})"), perl = TRUE)[[1]], collapse = " ")
      } else {
        word
      }
    })
    paste(broken_words, collapse = " ")
  }, USE.NAMES = FALSE)
}

create_summary_csv <- function(issues, env) {
  summary_df <- get_summary_df(issues)
  summary_df$issue_closer[is.na(summary_df$issue_closer)] <- "NA"
  summary_df$close_date[is.na(summary_df$close_date)] <- "NA"

  # wrap file paths
  summary_df$file_path <- insert_breaks(summary_df$file_path, 18)
  summary_df$file_path <- kableExtra::linebreak(summary_df$file_path)

  # wrap Author
  summary_df$author <- insert_breaks(summary_df$author, 28)
  summary_df$author <- kableExtra::linebreak(summary_df$author)

  summary_csv <- tempfile(fileext = ".csv")
  utils::write.csv(summary_df, file = summary_csv, row.names = FALSE)
  return(summary_csv)
}

create_intro <- function(repo, milestone_names) {
  author <- Sys.info()[["user"]]
  date <- format(Sys.Date(), '%B %d, %Y')
  milestone_names_list <- glue::glue_collapse(milestone_names, sep = ", ")

  image_path <- file.path(.le$config_repo_path, "logo.png")

  logo_exists_extra_header <- {
    if (fs::file_exists(image_path)) {
    glue::glue("
  - \\fancyhead[R]{{\\includegraphics[width=2cm, height=1.5cm, keepaspectratio]{{{image_path}}}}}
  - \\fancyhead[C]{{}}
  - \\fancyhead[L]{{}}
  - \"\\\\fancypagestyle{{plain}}{{\"
  - \"\\\\renewcommand{{\\\\headrulewidth}}{{0.4pt}}\"
  - \"}}\"", .trim = FALSE)
    }
    else {
      glue::glue("
  - \\fancyhead[R]{{}}
  - \\fancyhead[C]{{}}
  - \\fancyhead[L]{{}}", .trim = FALSE)
    }
  }

  intro <- glue::glue(
    "---
  title: \"QC Record: {milestone_names_list}\"
  subtitle: \"Git repository: {.le$repo}\"
  author: {author}
  date: {date}
  header-includes:
  - \\usepackage{{float}}
  - \\usepackage{{longtable}}
  - \\usepackage{{booktabs}}
  - \\usepackage{{makecell}}
  - \\usepackage{{graphicx}}
  - \\usepackage{{array}}
  - \\usepackage{{fancyhdr}}
  - \\usepackage{{xcolor}}
  - \\pagestyle{{fancy}}
  - \\newcolumntype{{R}}[1]{{>{{\\raggedright\\arraybackslash}}p{{#1}}}}
  - \\setlength{{\\headheight}}{{30pt}}
  - \\fancyfoot[C]{{Page \\thepage\\ of \\pageref{{LastPage}}}}
  - \\usepackage{{lastpage}}
  - \\lstset{{breaklines=true}}{logo_exists_extra_header}
  output:
    pdf_document:
      latex_engine: xelatex
      extra_dependencies: [\"xcolor\"]
      pandoc_args: --listings
      toc: true
      toc_depth: 2
---

  \\newpage

  ")

  return(intro)
}


set_up_chunk <- function() {
  glue::glue(
    "```{{r setup, include=FALSE}}
  library(knitr)
  library(dplyr)
  library(kableExtra)
  knitr::opts_chunk$set(eval=FALSE, warning = FALSE)\n```\n\n")
}

create_summary_table_section <- function(summary_csv) {
glue::glue(
"
```{{r, include=FALSE, eval=TRUE}}
summary_df <- read.csv(\"{summary_csv}\")\n

summary_df <- summary_df %>%
mutate(across(everything(), ~ ifelse(is.na(.), \"NA\", .)))
invisible(summary_df)
```

## **Issue Summary**
```{{r, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE}}
table <- summary_df %>%

knitr::kable(
  col.names = c(\"File Path\", \"Author\", \"QCer\", \"Issue Closer\", \"Close Date\"),
  format = \"latex\",
  booktabs = TRUE,
  escape = TRUE,
  longtable = TRUE,
  linesep = \"\\\\addlinespace\\\\addlinespace\"
) %>%
  kable_styling(latex_options = c(\"repeat_header\")) %>%
  column_spec(1, width = \"10em\") %>%
  column_spec(2, width = \"10em\")
```

```{{r, echo=FALSE, eval=TRUE, results='asis'}}
print(table)
```

\\newpage\n",
  .trim = FALSE
  )
}

create_set_of_issue_sections <- function(issues) {
  issue_numbers <- sapply(issues, function(issue) issue$number)
  issue_markdown_strings <- sapply(issues, function(issue) issue_to_markdown(issue$number))
  issue_titles <- sapply(issues, function(issue) issue$title)

  issue_section_strs <- mapply(create_medium_section, section_title = issue_titles, contents = issue_markdown_strings)
  issue_sections <- glue::glue_collapse(issue_section_strs, sep = "\n\\newpage\n")

  # now want to clean markdown for "---"
  issue_sections <- stringr::str_replace_all(issue_sections, stringr::fixed("---"), "`---`")
}

#' @importFrom log4r warn error info debug
create_milestone_report_section <- function(milestone_name, env, just_tables = FALSE) {
  debug(.le$logger, glue::glue("Creating section for Milestone: {milestone_name}..."))
  issues <- get_all_issues_in_milestone(milestone_name)

  debug(.le$logger, glue::glue("Creating summary table for Milestone: {milestone_name}..."))
  # summary table
  summary_csv <- create_summary_csv(issues, env)
  summary_table_section <- create_summary_table_section(summary_csv)
  info(.le$logger, glue::glue("Created summary table for Milestone: {milestone_name}"))
  # issues
  issue_sections <- create_set_of_issue_sections(issues)

  res <- {
    if (just_tables) {
      summary_table_section
    }
    else { # put it all together
      paste0(summary_table_section, issue_sections)
    }
  }
  info(.le$logger, glue::glue("Created section for Milestone: {milestone_name}"))
  return(res)

} # create_milestone_report_section

clean_input <- function(milestones_in) {
  # remove all quotes if any
  milestones_in_clean <- gsub('"', '', milestones_in)
  # make comma-separated str into vector
  milestones_list <- strsplit(milestones_in_clean, ",\\s*")
  unlist(milestones_list)
}

get_inputted_milestone_names <- function() {
  # gate with interactive() to avoid hanging
  if (interactive()) {

    milestones <- list_ghqc_milestone_names()
    print(glue::glue("Non-empty Milestones in {.le$repo}:\n"))
    print(milestones)
    valid_input <- FALSE
    while (!valid_input) {
      # read in milestones
      user_input <- readline(prompt = glue::glue("\nInput Milestones: e.g. milestone1, milestone2: "))
      clean_input <- clean_input(user_input)

      # check they exist and are non-empty
      result <- tryCatch({
        check_milestones(clean_input)
        TRUE
      },
      warning = function(w) {
        warning(w$message)
        FALSE
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        FALSE
      })

      # Check if the conversion was successful
      if (result) {
        cat("You entered valid milestones:", glue::glue_collapse(clean_input, sep = ", "), "\n")
        valid_input <- TRUE
      }
      else {
        cat("Invalid input. Please try again.\n")
      }
    }
    return(clean_input)
  }
} # get_inputted_milestone_names

check_milestones <- function(milestone_names) {
  # check that each milestone exists and is non-empty
  lapply(milestone_names, function(milestone_name) {
    exists <- milestone_exists(milestone_name)
    if (!exists) {
      stop(glue::glue("\"{milestone_name}\" is not a Milestone in {.le$repo}"))
    }
    milestone <- get_milestone_from_name(milestone_name)
    non_empty <- check_that_milestone_is_non_empty(milestone)
    if (!non_empty) {
      stop(glue::glue("\"{milestone_name}\" in {.le$repo} is an empty Milestone (no issues)"))
    }
  })
}

unchecked_items_in_issue <- function(issue) {
  unchecked_items <- stringr::str_detect(issue$body, "- \\[ \\]")
}

create_milestone_table <- function(milestone_names) {
  milestone_df <- create_milestone_df(milestone_names)
  milestone_csv <- create_milestone_csv(milestone_df)

  glue::glue(
    "
```{{r, include=FALSE, eval=TRUE}}
milestone_df <- read.csv(\"{milestone_csv}\")\n

milestone_df <- milestone_df %>%
mutate(across(everything(), ~ ifelse(is.na(.), \"NA\", .)))
invisible(milestone_df)
```

# Milestone Summary
```{{r, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}}

table <- milestone_df %>%
knitr::kable(
  col.names = c(\"Title\", \"Description\", \"Status\", \"Issues\"),
  format = \"latex\",
  booktabs = TRUE,
  escape = FALSE,
  longtable = TRUE,
  linesep = \"\\\\addlinespace\\\\addlinespace\"
) %>%
  kable_styling(latex_options = c(\"repeat_header\")) %>%
  footnote(general=c(\"\\\\\\\\textcolor{{red}}{{O}} Open Issue\", \"\\\\\\\\textcolor{{green}}{{U}} Issue with unchecked items\"), general_title = \"\", escape = FALSE) %>%
  column_spec(1, width = \"0.20\\\\\\\\textwidth\", latex_valign = \"p\") %>%
  column_spec(2, width = \"0.20\\\\\\\\textwidth\", latex_valign = \"p\") %>%
  column_spec(4, width = \"0.43\\\\\\\\textwidth\", latex_valign = \"p\")
```

```{{r, echo=FALSE, eval=TRUE, results='asis'}}
print(table)
```

\\newpage\n",
    .trim = FALSE
  )
}

create_milestone_csv <- function(milestone_df) {
  milestone_csv <- tempfile(fileext = ".csv")
  #suppressMessages({withr::defer(fs::file_delete(milestone_csv))})
  utils::write.csv(milestone_df, file = milestone_csv, row.names = FALSE)
  return(milestone_csv)
}


create_milestone_df <- function(milestone_names) {
  milestone_objects <- lapply(milestone_names, function(milestone_name) {
    get_milestone_from_name(milestone_name)
  })

  issues_in_milestones <- sapply(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(milestone_name)
    issue_names <- lapply(issues, function(issue) {
      issue_name <- issue$title
      # insert line breaks here before adding makecell and additional chars

      issue_name <- insert_breaks(issue_name, 42)

      if (issue$state == "open") {
        issue_name <- glue::glue("{issue_name}\\textcolor{{red}}{{O}}")
      }
      if (unchecked_items_in_issue(issue)) {
        issue_name <- glue::glue("{issue_name}\\textcolor{{green}}{{U}}")
      }

      return(issue_name)
    })

    issues_str <- glue::glue_collapse(issue_names, "\n\n")
    issues_str <- kableExtra::linebreak(issues_str)
    issues_str <- stringr::str_replace_all(issues_str, "_", "\\\\_")
    return(issues_str)
  })

  milestone_statuses <- sapply(milestone_objects, function(milestone) {
    milestone$state
  })

  milestone_descriptions <-  sapply(milestone_objects, function(milestone) {
    desc <- milestone$description
    if (is.null(desc)) {
      desc <- "NA"
    }

    desc <- insert_breaks(desc, 20)
    desc <- kableExtra::linebreak(desc)
    desc <- stringr::str_replace_all(desc, "_", "\\\\_")
    return(desc)
  })

  milestone_names <- sapply(milestone_names, function(milestone_name) {
    milestone_name <- insert_breaks(milestone_name, 18)
    milestone_name <- kableExtra::linebreak(milestone_name)
    milestone_name <- stringr::str_replace_all(milestone_name, "_", "\\\\_")

  })

  milestone_df <- data.frame(
    name = milestone_names,
    description = milestone_descriptions,
    status = milestone_statuses,
    issues = issues_in_milestones
  )

  milestone_df
}


#' @importFrom log4r warn error info debug
ghqc_report <- function(milestone_names = NULL,
                        input_name = NULL,
                        just_tables = FALSE,
                        location = ".") {

  # get user input if milestone_names not inputted (check existence here)
  if (is.null(milestone_names)) {
    milestone_names <- get_inputted_milestone_names()
  }
  else {
    # check that milestones exist and are non-empty
    check_milestones(milestone_names)
  }

  if (fs::is_file(location)) {
    error(.le$logger, glue::glue("Inputted directory {location} is a file path. Input an existing directory."))
    rlang::abort(message = glue::glue("Inputted directory {location} is a file path.<br>Input an existing directory."))
  }

  # check location exists
  if (!fs::dir_exists(location)) {
    error(.le$logger, glue::glue("Inputted directory {location} doesn't exist. Input an existing directory."))
    rlang::abort(message = glue::glue("Inputted directory {location} doesn't exist.<br>Input an existing directory."))
  }

  debug(.le$logger, "Creating QC Record introduction...")
  # intro
  intro <- create_intro(repo, milestone_names)
  set_up_chunk <- set_up_chunk()
  info(.le$logger, "Created QC Record introduction")

  # create milestone table
  milestone_table <- create_milestone_table(milestone_names)

  debug(.le$logger, "Creating Milestone sections...")
  # create milestone sections
  milestone_sections <- lapply(milestone_names, function(milestone_name) {
    milestone_body <- create_milestone_report_section(milestone_name, parent.frame(n = 2), just_tables)
    create_big_section(milestone_name, milestone_body)
  })
  info(.le$logger, "Created Milestone sections")

  # appendix

  rmd_content <- glue::glue_collapse(c(intro,
                                       set_up_chunk,
                                       milestone_table,
                                       milestone_sections
                                       ), sep = "")

  pdf_name <- get_pdf_name(input_name = input_name,
                           milestone_names = milestone_names,
                           just_tables = just_tables,
                           repo = repo)

  # create pdf from markdown

  debug(.le$logger, "Converting rmd to pdf...")

  suppressWarnings(
    markdown_to_pdf(rmd_content = rmd_content,
                    repo = repo,
                    milestone_names = milestone_names,
                    just_tables = just_tables,
                    location = location,
                    pdf_name = pdf_name)
  )

}

get_simple_path <- function(working_dir = gert::git_find()) {
  home_dir <- Sys.getenv("HOME")
  simple_path <- stringr::str_replace(working_dir,
                                      stringr::fixed(home_dir),
                                      "~")
  return(simple_path)
}
