create_qc_data_section <- function(issue_creation_time, issue_creator, issue_title, issue_number, milestone_title, milestones) {
  sections <- c()
  # get qc_initializer
  humanized_creation_time <- humanize_time(issue_creation_time)
  qc_initializer <- glue::glue("{issue_creator} at {humanized_creation_time}")

  # get file author
  authors <- get_authors(issue_title)
  latest_author <- authors$latest
  author_section <- glue::glue("* **File Author:** {latest_author}")
  sections <- c(sections, author_section)

  # get collaborators
  collaborators <- authors$collaborators
  if (length(collaborators) != 0) {
    collaborators <- glue::glue_collapse(collaborators, sep = ", ")
    collaborators_section <- glue::glue("* **Other file collaborators:** {collaborators}")
    sections <- c(sections, collaborators_section)
  }

  # issue number and qc_init sections
  qc_init_section <- glue::glue("* **QC initializer:** {qc_initializer}")
  issue_number_section <- glue::glue("* **Issue number:** {issue_number}")
  milestone_section <- create_milestone_section(milestone_title, milestones)
  sections <- c(sections, qc_init_section, issue_number_section, milestone_section)

  # create body
  issue_body <- glue::glue_collapse(sections, sep = "\n\n")
  issue_section <- create_small_section("QC Data", issue_body)
}

create_milestone_section <- function(milestone_title, milestones) { # issue$milestone$title
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
    #create_section("Milestone", milestone_body)
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

# clean_body <- function(body) {
#   body <- stringr::str_replace_all(body, "(^#{1,4} (\\w+)|\n#{1,4} (\\w+))", function(x) {
#     word <- stringr::str_extract(x, "(?<=#{1,4} )\\w+")
#     paste0("**", word, "**")
#   })
#   return(body)
# }

clean_body <- function(body) {
  body <- stringr::str_replace_all(body, "(^#{1,4} .*|\\n#{1,4} .*)", function(x) {
    line <- stringr::str_extract(x, "(?<=#{1,4} ).*")
    line <- stringr::str_trim(line)
    paste0("**", line, "**")
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

issue_to_markdown <- function(owner, repo, issue_number) {
  # collect issue info
  issue <- get_issue(owner, repo, issue_number)
  milestones <- get_all_milestone_objects(owner, repo)

  issue_comments <- get_issue_comments(owner, repo, issue_number)

  issue_events <- get_issue_events(owner, repo, issue_number)
  events_list <- get_events_list(issue_events)

  timeline <- get_issue_timeline(owner, repo, issue_number)

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
        glue::glue("tables-{repo}-{milestone_str}")
      }
      else {
        glue::glue("{repo}-{milestone_str}")
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
  debug(.le$logger, "Creating Record pdf...")
  # create temporary rmd
  rmd <- tempfile(fileext = ".Rmd")

  #rmd <- file.path(location, "report.Rmd")
  fs::file_create(rmd)
  debug(.le$logger, glue::glue("Rmd location: {rmd}"))
  # delete temporary rmd when it's time
 #suppressMessages({withr::defer_parent(fs::file_delete(rmd))})
  writeLines(rmd_content, con = rmd)

  # create pdf from rmd
  location <- normalizePath(location)
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

  #suppressMessages({withr::defer_parent(unlink(dirname(rmd)))})

  pdf_path_abs <- get_simple_path(output_file)

  info(.le$logger, "Converted rmd to pdf")
  info(.le$logger, glue::glue("Created Record pdf: {pdf_path_abs}"))

  return(pdf_path_abs)
} # markdown_to_pdf

scrape_issue <- function(owner, repo, issue_number) {
  rmd_contents <- issue_to_markdown(owner, repo, issue_number)
  markdown_to_pdf(rmd_contents, repo, issue_number)
} # scrape

get_summary_table_col_vals <- function(issue) {
  metadata <- {
    tryCatch({
      get_metadata(issue$body)
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
  author <- ifelse(!is.null(latest_author), latest_author, "NA")
  qc_type <- ifelse(!is.null(metadata$`qc type`), metadata$`qc type`, ifelse(!is.null(metadata$`qc_type`), "NA"))
  #file_name <- basename(file_path)
  #git_sha <- ifelse(!is.null(metadata$git_sha), metadata$git_sha, NA)
  qcer <- ifelse(length(issue$assignees) > 0, issue$assignees[[1]], "NA")
  issue_closer <- ifelse(!is.null(close_data$closer), close_data$closer, "NA")
  close_date <- ifelse(!is.null(close_data$closed_at), close_data$closed_at, "NA")

  c(
    file_path = file_path,
    author = author,
    qc_type = qc_type,
    #file_name = file_name,
    #git_sha = git_sha,
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
    if (nchar(x) > width) {
      # insert spaces into long words
      paste(strsplit(x, paste0("(?<=.{", width, "})"), perl = TRUE)[[1]], collapse = "-")
    } else {
      x
    }
  })
}

create_summary_csv <- function(issues, env) {
  summary_df <- get_summary_df(issues)
  summary_df$issue_closer[is.na(summary_df$issue_closer)] <- "NA"
  summary_df$close_date[is.na(summary_df$close_date)] <- "NA"
  # summary_df <- data.frame(
  #   file_path = c("scripts/sub_dir/long_titled_R_script_for_a_test.R", "short_file.txt", "mediummediummedium_file.R"),
  #   author = c("jenna-a2ai <jenna@a2-ai.com>", "longergithubname <longergithubname@a2-ai.com", "longerlongerlongergithubname <longerlongerlongergithubname@a2-ai.com"),
  #   qc_type = c("mrgsolve Model Validation", "qctypeqctypeqctypeqctypeqctypeqctype", "qctype qctype qctype qctype qctype "),
  #   qcer = c("jenna-a2ai", "longernamelongernamelongername", "mediumname"),
  #   issue_closer = c("jenna-a2ai", "mediumname", "longernamelongernamelongername"),
  #   close_date = c("2024-09-18 18:34:50", "2024-09-18 18:34:50", "2024-09-18 18:34:50")
  # )
  # wrap file paths
  summary_df$file_path <- insert_breaks(summary_df$file_path, 17)
  summary_df$author <- insert_breaks(summary_df$author, 28)
  #summary_df$qc_type <- insert_breaks(summary_df$qc_type, 25)
  summary_df$qcer <- insert_breaks(summary_df$qcer, 10)
  summary_df$issue_closer <- insert_breaks(summary_df$issue_closer, 10)
  summary_df$close_date <- insert_breaks(summary_df$close_date, 20)

  summary_csv <- tempfile(fileext = ".csv")
  #suppressMessages({withr::defer(fs::file_delete(summary_csv), env)})
  #summary_csv <- file.path(getwd(), "summary.csv")
  utils::write.csv(summary_df, file = summary_csv, row.names = FALSE)
  return(summary_csv)
}

create_intro <- function(repo, milestone_names) {
  author <- Sys.info()[["user"]]
  date <- format(Sys.Date(), '%B %d, %Y')
  milestone_names_list <- glue::glue_collapse(milestone_names, sep = ", ")
  #
  image_path <- file.path(.le$info_repo_path, "logo.png")
  intro <- glue::glue(
    "---
  title: \"QC Record: {milestone_names_list}\"
  subtitle: \"Git repository: {repo}\"
  author: {author}
  date: {date}
  header-includes:
  - \\usepackage{{booktabs}}
  - \\usepackage{{makecell}}
  - \\usepackage{{graphicx}}
  - \\usepackage{{pdflscape}}
  - \\usepackage{{array}}
  - \\usepackage{{fancyhdr}}
  - \\usepackage{{xcolor}}
  - \\pagestyle{{fancy}}
  - \\newcolumntype{{R}}[1]{{>{{\\raggedright\\arraybackslash}}p{{#1}}}}
  - \\newcommand{{\\blandscape}}{{\\begin{{landscape}}}}
  - \\newcommand{{\\elandscape}}{{\\end{{landscape}}}}
  - \\fancyhead[R]{{\\includegraphics[width=2cm]{{{image_path}}}}}
  - \\fancyhead[C]{{}}
  - \\fancyhead[L]{{}}
  - \\setlength{{\\headheight}}{{30pt}}
  - \\fancyfoot[C]{{Page \\thepage\\ of \\pageref{{LastPage}}}}
  - \\usepackage{{lastpage}}
  - \\lstset{{breaklines=true}}
  - \"\\\\fancypagestyle{{plain}}{{\"
  - \"\\\\fancyhead[R]{{\\\\includegraphics[width=2cm]{{{image_path}}}}}\"
  - \"\\\\renewcommand{{\\\\headrulewidth}}{{0.4pt}}\"
  - \"}}\"
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
  col.names = c(\"File Path\", \"Author\", \"QC Type\", \"QCer\", \"Issue Closer\", \"Close Date\"),
  format = \"latex\",
  booktabs = TRUE,
  escape = TRUE,
  linesep = \"\\\\addlinespace\\\\addlinespace\"
) %>%
  kable_styling(latex_options = c(\"hold_position\", \"scale_down\")) %>%
  column_spec(1, width = \"10em\") %>%
  column_spec(2, width = \"14em\") %>%
  column_spec(3, width = \"12em\") %>%
  column_spec(4, width = \"6em\") %>%
  column_spec(5, width = \"6em\") %>%
  column_spec(6, width = \"9em\")

```

```{{r, echo=FALSE, eval=TRUE, results='asis'}}
print(table)
```

\\newpage\n",
  .trim = FALSE
  )
}

create_set_of_issue_sections <- function(issues, owner, repo) {
  issue_numbers <- sapply(issues, function(issue) issue$number)
  issue_markdown_strings <- sapply(issues, function(issue) issue_to_markdown(owner, repo, issue$number))
  issue_titles <- sapply(issues, function(issue) issue$title)

  issue_section_strs <- mapply(create_medium_section, section_title = issue_titles, contents = issue_markdown_strings)
  issue_sections <- glue::glue_collapse(issue_section_strs, sep = "\n\\newpage\n")
}

#' @importFrom log4r warn error info debug
create_milestone_report_section <- function(owner, repo, milestone_name, env, just_tables = FALSE) {
  debug(.le$logger, glue::glue("Creating section for Milestone: {milestone_name}..."))
  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

  debug(.le$logger, glue::glue("Creating summary table for Milestone: {milestone_name}..."))
  # summary table
  summary_csv <- create_summary_csv(issues, env)
  summary_table_section <- create_summary_table_section(summary_csv)
  info(.le$logger, glue::glue("Created summary table for Milestone: {milestone_name}"))
  # issues
  issue_sections <- create_set_of_issue_sections(issues, owner, repo)

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

get_inputted_milestone_names <- function(owner, repo) {
  # gate with interactive() to avoid hanging
  if (interactive()) {

    milestones <- list_milestones(owner, repo)
    print(glue::glue("Non-empty Milestones in {repo}:\n"))
    print(milestones)
    valid_input <- FALSE
    while (!valid_input) {
      # read in milestones
      user_input <- readline(prompt = glue::glue("\nInput Milestones: e.g. milestone1, milestone2: "))
      clean_input <- clean_input(user_input)

      # check they exist and are non-empty
      result <- tryCatch({
        check_milestones(clean_input, owner, repo)
        TRUE
      },
      warning = function(w) {
        warning(w$message)
        FALSE
      },
      error = function(e) {
        cat("Error:", e$message, "\n")
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

check_milestones <- function(milestone_names, owner, repo) {
  # check that each milestone exists and is non-empty
  lapply(milestone_names, function(milestone_name) {
    exists <- milestone_exists(milestone_name, owner, repo)
    if (!exists) {
      stop(glue::glue("\"{milestone_name}\" is not a Milestone in {repo}"))
    }
    milestone <- get_milestone_from_name(owner, repo, milestone_name)
    non_empty <- check_that_milestone_is_non_empty(milestone)
    if (!non_empty) {
      stop(glue::glue("\"{milestone_name}\" in {repo} is an empty Milestone (no issues)"))
    }
  })
}

unchecked_items_in_issue <- function(issue) {
  unchecked_items <- stringr::str_detect(issue$body, "- \\[ \\]")
}

create_milestone_table <- function(milestone_names, owner, repo) {
  milestone_df <- create_milestone_df(milestone_names, owner, repo)
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
```{{r, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE}}

table <- milestone_df %>%
knitr::kable(
  col.names = c(\"Title\", \"Description\", \"Status\", \"Issues\"),
  format = \"latex\",
  booktabs = TRUE,
  escape = FALSE,
  linesep = \"\\\\addlinespace\\\\addlinespace\"
) %>%
  kable_styling(latex_options = c(\"hold_position\", \"scale_down\")) %>%
  footnote(general=c(\"\\\\\\\\textcolor{{red}}{{O}} Open Issue\", \"\\\\\\\\textcolor{{green}}{{U}} Issue with unchecked items\"), general_title = \"\", escape = FALSE) %>%
  column_spec(1, width = \"5em\", latex_valign = \"p\") %>%
  column_spec(2, width = \"10em\", latex_valign = \"p\") %>%
  column_spec(3, width = \"3em\", latex_valign = \"p\") %>%
  column_spec(4, width = \"22em\", latex_valign = \"p\")
  #collapse_rows(1:4, valign = \"top\")

  #row_spec(1:4, latex_valign = \"t\")

  #column_spec(1, width = \"10em\")
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


create_milestone_df <- function(milestone_names, owner, repo) {
  milestone_objects <- lapply(milestone_names, function(milestone_name) {
    get_milestone_from_name(owner, repo, milestone_name)
  })

  issues_in_milestones <- sapply(milestone_names, function(milestone_name) {
    issues <- get_all_issues_in_milestone(owner, repo, milestone_name)
    issue_names <- lapply(issues, function(issue) {
      issue_name <- issue$title
      # insert line breaks here before adding makecell and additional chars
      # milestone_df$title <- insert_breaks(milestone_df$title, 10)
      # milestone_df$description <- insert_breaks(milestone_df$description, 10)
      # milestone_df$status <- insert_breaks(milestone_df$status, 10)

      issue_name <- insert_breaks(issue_name, 45)

      if (issue$state == "open") {
        issue_name <- glue::glue("{issue_name}\\textcolor{{red}}{{O}}")
      }
      if (unchecked_items_in_issue(issue)) {
        issue_name <- glue::glue("{issue_name}\\textcolor{{green}}{{U}}")
      }

      return(issue_name)
    })

    issues_str <- glue::glue_collapse(issue_names, "\n")
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

    desc <- insert_breaks(desc, 23) #LB
    desc <- kableExtra::linebreak(desc)
    desc <- stringr::str_replace_all(desc, "_", "\\\\_")
    return(desc)
  })

  milestone_names <- sapply(milestone_names, function(milestone_name) {
    milestone_name <- insert_breaks(milestone_name, 10) #LB
    milestone_name <- kableExtra::linebreak(milestone_name)
    milestone_name <- stringr::str_replace_all(milestone_name, "_", "\\\\_")
  })


  milestone_df <- data.frame(
    name = milestone_names,
    description = milestone_descriptions,
    status = milestone_statuses,
    issues = issues_in_milestones
  )

  # milestone_df$issues <- stringr::str_replace_all(milestone_df$issues, "_", "\\\\_")
  # milestone_df$name <- stringr::str_replace_all(milestone_df$name, "_", "\\\\_")
  # milestone_df$description <- stringr::str_replace_all(milestone_df$description, "_", "\\\\_")

  milestone_df
}


#' @importFrom log4r warn error info debug
ghqc_report <- function(milestone_names = NULL,
                        input_name = NULL,
                        just_tables = FALSE,
                        location = ".",
                        owner = get_organization(),
                        repo = get_current_repo()) {

  # get user input if milestone_names not inputted (check existence here)
  if (is.null(milestone_names)) {
    milestone_names <- get_inputted_milestone_names(owner, repo)
  }
  else {
    # check that milestones exist and are non-empty
    check_milestones(milestone_names, owner, repo)
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

  debug(.le$logger, "Creating Record introduction...")
  # intro
  intro <- create_intro(repo, milestone_names)
  set_up_chunk <- set_up_chunk()
  info(.le$logger, "Created Record introduction")

  # create milestone table
  milestone_table <- create_milestone_table(milestone_names, owner, repo)

  debug(.le$logger, "Creating Milestone sections...")
  # create milestone sections
  milestone_sections <- lapply(milestone_names, function(milestone_name) {
    milestone_body <- create_milestone_report_section(owner, repo, milestone_name, parent.frame(n = 2), just_tables)
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
