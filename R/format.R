
format_issue_body <- function(checklist_type, file_path) {
  checklists <- get_checklists()
  file_items <- checklists[[checklist_type]]
  qc_checklist <- format_checklist_items(file_items)
  metadata <- format_metadata(checklist_type, file_path)
  note <- format_note()

  issue_body_content <- format_body_content(metadata = metadata,
                                            checklist_type = checklist_type,
                                            note = note,
                                            qc_checklist = qc_checklist)
}

format_items <- function(items) {
  formatted_items <- sapply(items, function(item) {
    glue::glue("- [ ] {item}")
  })
  glue::glue_collapse(formatted_items, sep = '\n')
}

format_section_list <- function(section_name, items) {
  formatted_items <- format_items(items)

  glue::glue("### {section_name}\n\n{formatted_items}\n\n")
}

# functions to format body of Issue
format_checklist_items <- function(checklist) {
  names <- names(checklist)
  # if no sub-headers
  if (is.null(names)) {
    return(format_items(checklist))
  }
  # else, sub-headers
  else {
    checklist_sections <- lapply(names, function(section_name) {
      section <- checklist[[section_name]]
      format_section_list(section_name, section)
    })
    return(glue::glue_collapse(checklist_sections, sep = '\n'))
  }
}

get_sha <- function() {
  commits <- gert::git_log()
  commits$commit[1]
}


get_authors <- function(file_path) {
  # https://stackoverflow.com/questions/11533199/how-to-find-the-commit-in-which-a-given-file-was-added

  # shell out
  # git log --follow -- file_path
  log <- processx::run("git", c("log", "--follow", "--", file_path))$stdout
  # get lines with author
  author_lines <- unlist(stringr::str_extract_all(log, "Author:.*"))
  # remove "Author: " prefix
  authors <- stringr::str_remove(author_lines, "Author: ")
  # get most recent author
  latest_author <- authors[1]
  # get unique authors
  unique_authors <- unique(stringr::str_remove(authors, "Author: "))
  # get collaborators besides most recent author
  other_collaborators <- unique_authors[unique_authors != latest_author]

  list(latest = latest_author,
       collaborators = other_collaborators)
}

format_collaborators <- function(collaborators, prefix = "") {
  if (length(collaborators) > 0) {
    collaborators_cat <- glue::glue_collapse(collaborators, ", ")
    glue::glue("{prefix}{collaborators_cat}")
  }
  else {
    ""
  }
}

format_metadata <- function(checklist_type, file_path) {
  authors <- get_authors(file_path)
  latest_author <- authors$latest
  author_section <- glue::glue("* author: {latest_author}")
  metadata <- c(author_section)

  collaborators_section <- format_collaborators(authors$collaborators, prefix = "* collaborators: ")
  if (collaborators_section != "") {
    metadata <- c(metadata, collaborators_section)
  }

  qc_type <- checklist_type
  qc_type_section <- glue::glue("* qc type: {qc_type}")

  script_hash <- digest::digest(file = file_path)
  script_hash_section <- glue::glue("* md5 checksum: {script_hash}")

  git_sha <- get_sha()
  git_sha_section <- glue::glue("* initial qc commit: {git_sha}")

  file_history_url <- get_file_history_url(file_path)
  file_history_url_section <- glue::glue("* file history: {file_history_url}")

  metadata <- c(git_sha_section, metadata, qc_type_section, script_hash_section, file_history_url_section)

  glue::glue_collapse(metadata, "\n")
}

get_file_history_url <- function(file_path) {
  # get branch
  branch <- gert::git_branch()

  # get remote url (assume first row)
  remote_url <- gert::git_remote_list()$url[1]

  # if it's an ssh, construct manually
  if (grepl("^git@", remote_url)) {
    # get the domain and repo
    domain <- sub("git@(.*):.*", "\\1", remote_url)
    repo_path <- sub("git@.*:(.*)", "\\1", remote_url)

    remote_url <- glue::glue("https://{domain}/{repo_path}")
  }

  # take out .git at the end
  https_url <- sub(".git$", "", remote_url)

  # get something like https://github.com/A2-ai/project_x/commits/main/scripts/DA.R
  file_history_url <- glue::glue("{https_url}/commits/{branch}/{file_path}")
}

format_note <- function() {
  note <- {
    if (file.exists(file.path(.le$info_repo_path, "note"))) {
      custom_note <- readr::read_file(file.path(.le$info_repo_path, "note"))
      if (stringr::str_sub(custom_note, start =-2) != "\n") {
        custom_note <- paste0(custom_note,"\n")
      }
      custom_note
    }
    else {
      ""
    }
  }

  return(note)
}

format_body_content <- function(metadata, checklist_type, note, qc_checklist) {
  glue::glue("## Metadata\n\n
             {metadata}\n\n
             # {checklist_type}\n\n
             {note}\n\n
             {qc_checklist}")
}
