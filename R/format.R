
format_issue_body <- function(checklist_type, file_path, relevant_files, owner, repo, remote, file_names) {
  remote_url <- parse_remote_url(remote$url)

  checklists <- get_checklists()
  file_items <- checklists[[checklist_type]]
  qc_checklist <- format_checklist_items(file_items)

  metadata <- format_metadata(checklist_type, file_path, owner, repo, remote_url)

  relevant_files <- format_relevant_files(relevant_files, owner, repo, remote_url, file_names)

  note <- get_prepended_checklist_note()

  issue_body_content <- format_body_content(metadata = metadata,
                                            relevant_files = relevant_files,
                                            checklist_type = checklist_type,
                                            note = note,
                                            qc_checklist = qc_checklist)
}

format_relevant_files <- function(relevant_files, owner, repo, remote_url, file_names) {
  if (is.null(relevant_files)) {
    return("")
  }

  file_sections <- lapply(relevant_files, function(file) {
    qced_separately <- ifelse(file$file_path %in% file_names, TRUE, FALSE)

    file_name <- ifelse(is.null(file$name) || file$name == "", file$file_path, file$name)
    branch <- gert::git_branch()
    file_contents_url <- file.path(remote_url, owner, repo, "blob", branch, file$file_path)

    file_section <- glue::glue(
      "- **{file_name}**\n   - [`{file$file_path}`]({file_contents_url})", .trim = FALSE
    )

    if(qced_separately) {
      file_section <- glue::glue(file_section,
                                 "\n   - QCed separately in another Issue",  .trim = FALSE
                                 )
    }

    if (!is.null(file$note) && file$note != "") {
      modified_note <- stringr::str_replace_all(file$note, "\\n", "\\\n      > ")
      file_section <- glue::glue(file_section,
                                 "\n      > {modified_note}", .trim = FALSE)
    }
    else {
      file_section <- glue::glue(file_section,
                                 "\n&nbsp;", .trim = FALSE)
    }

    file_section
  })

  file_sections_col <- glue::glue_collapse(file_sections, sep = "\n")
  # if ended on a &nbsp, remove it for nicer formatting between sections
  file_sections_col <- stringr::str_remove(file_sections_col, "&nbsp;$")

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

  glue::glue("### {section_name}\n\n{formatted_items}\n\n\n", .trim = FALSE)
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

format_metadata <- function(checklist_type, file_path, owner, repo, remote_url) {
  authors <- get_authors(file_path)
  latest_author <- authors$latest
  author_section <- glue::glue("* author: {latest_author}")
  metadata <- c(author_section)

  collaborators_section <- format_collaborators(authors$collaborators, prefix = "* collaborators: ")
  if (collaborators_section != "") {
    metadata <- c(metadata, collaborators_section)
  }

  git_sha <- get_sha()
  git_sha_section <- glue::glue("* initial qc commit: {git_sha}")

  git_branch <- get_branch_url(git_sha, owner, repo, remote_url)
  git_branch_section <- glue::glue("* git branch: {git_branch}")

  file_contents_url <- get_file_contents_url(file_path, git_sha, owner, repo, remote_url)
  file_content_url_section <- glue::glue("* file contents at initial qc commit: {file_contents_url}")

  script_hash <- digest::digest(file = file_path)
  script_hash_section <- glue::glue("* md5 checksum: {script_hash}")

  metadata <- c(git_sha_section, git_branch_section, metadata, script_hash_section, file_content_url_section)

  glue::glue_collapse(metadata, "\n")
}

get_branch_url <- function(git_sha, owner, repo, remote_url) {
  branch <- gert::git_branch()
  https_url <- file.path(remote_url, owner, repo, "tree", git_sha)

  glue::glue("[{branch}]({https_url})")
}

get_file_history_url <- function(file_path, owner, repo, remote_url) {
  branch <- gert::git_branch()
  file_path <- gsub(" ", "%20", file_path)

  # get something like https://github.com/A2-ai/project_x/commits/main/scripts/DA.R
  file_history_url <- file.path(remote_url, owner, repo, "commits", branch, file_path)
}

get_file_contents_url <- function(file_path, git_sha, owner, repo, remote_url) {
  file_path <- gsub(" ", "%20", file_path)

  file_contents_url <- file.path(remote_url, owner, repo, "blob", substr(git_sha, 1, 6), file_path)
}

format_body_content <- function(metadata, checklist_type, note, qc_checklist, relevant_files) {
  glue::glue("## Metadata\n\n
             {metadata}\n\n
             {relevant_files}
             # {checklist_type}\n\n
             {note}\n\n
             {qc_checklist}")
}
