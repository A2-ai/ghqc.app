
format_issue_body <- function(checklist_type, file_path, relevant_files, owner, repo) {
  checklists <- get_checklists()
  file_items <- checklists[[checklist_type]]
  qc_checklist <- format_checklist_items(file_items)
  metadata <- format_metadata(checklist_type, file_path, owner, repo)
  relevant_files <- format_relevant_files(relevant_files)
  note <- get_prepended_checklist_note()

  issue_body_content <- format_body_content(metadata = metadata,
                                            relevant_files = relevant_files,
                                            checklist_type = checklist_type,
                                            note = note,
                                            qc_checklist = qc_checklist)
}

format_relevant_files <- function(relevant_files) {

  if (is.null(relevant_files)) {
    return("")
  }

  file_sections <- lapply(relevant_files, function(file) {
    file_name <- ifelse(is.null(file$name) || file$name == "", file$file_path, file$name)

    file_section <- glue::glue(
      "- **{file_name}**\n   - [`{file$file_path}`](https://github.com)", .trim = FALSE
    )

    if (!is.null(file$note) && file$note != "") {
      file_section <- glue::glue(file_section,
                                 "\n   - {file$note}", .trim = FALSE)
    }
    file_section
  })

  file_sections_col <- glue::glue_collapse(file_sections, sep = "\n&nbsp;\n")

  relevant_files_section <- glue::glue(
    "## Relevant files

    {file_sections_col}

    "
  )

  relevant_files_section
  return(relevant_files_section)

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

format_metadata <- function(checklist_type, file_path, owner, repo) {
  authors <- get_authors(file_path)
  latest_author <- authors$latest
  author_section <- glue::glue("* author: {latest_author}")
  metadata <- c(author_section)

  collaborators_section <- format_collaborators(authors$collaborators, prefix = "* collaborators: ")
  if (collaborators_section != "") {
    metadata <- c(metadata, collaborators_section)
  }

  # qc_type <- checklist_type
  # qc_type_section <- glue::glue("* qc type: {qc_type}")

  git_sha <- get_sha()
  git_sha_section <- glue::glue("* initial qc commit: {git_sha}")

  git_branch <- get_branch_url(git_sha, owner, repo)
  git_branch_section <- glue::glue("* git branch: {git_branch}")

  # file_history_url <- get_file_history_url(file_path)
  # file_history_url_section <- glue::glue("* file history: {file_history_url}")

  file_contents_url <- get_file_contents_url(file_path, git_sha)
  file_content_url_section <- glue::glue("* file contents at initial qc commit: {file_contents_url}")

  script_hash <- digest::digest(file = file_path)
  script_hash_section <- glue::glue("* md5 checksum: {script_hash}")

  metadata <- c(git_sha_section, git_branch_section, metadata, script_hash_section, file_content_url_section)

  glue::glue_collapse(metadata, "\n")
}

get_branch_url <- function(git_sha, owner, repo) {
  branch <- gert::git_branch()
  remote <- get_remote()
  remote_url <- parse_remote_url(remote$url)

  https_url <- file.path(remote_url, owner, repo, "tree", git_sha)

  glue::glue("[{branch}]({https_url})")
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

  file_path <- gsub(" ", "%20", file_path)

  # get something like https://github.com/A2-ai/project_x/commits/main/scripts/DA.R
  file_history_url <- glue::glue("{https_url}/commits/{branch}/{file_path}")
}

get_file_contents_url <- function(file_path, git_sha) {
  # branch <- gert::git_branch()
  #remote_url <-  gert::git_remote_list()$url[1]
  browser()
  remote_url <- parse_remote_url(get_remote()$url)

  # if (grepl("^git@", remote_url)) {
  #   # get the domain and repo
  #   domain <- sub("git@(.*):.*", "\\1", remote_url)
  #   repo_path <- sub("git@.*:(.*)", "\\1", remote_url)
  #
  #   remote_url <- glue::glue("https://{domain}/{repo_path}")
  # }

  # take out .git at the end
  #https_url <- sub(".git$", "", remote_url)

  file_path <- gsub(" ", "%20", file_path)

  # file.path(https_url, "blob", branch, file_path)
  file.path(remote_url, "blob", substr(git_sha, 1, 6), file_path)
}

format_body_content <- function(metadata, checklist_type, note, qc_checklist, relevant_files) {
  glue::glue("## Metadata\n\n
             {metadata}\n\n
             {relevant_files}
             # {checklist_type}\n\n
             {note}\n\n
             {qc_checklist}")
}
