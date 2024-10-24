check_item <- function(item) {
  # replace [ ] in item with [x]
  gsub("\\[ \\]", "\\[x\\]", item)
}

uncheck_item <- function(item) {
  # replace [x] in item with [ ]
  gsub("\\[x\\]", "\\[ \\]", item)
}


# items is a list, items_to_check is a vector of sub-strings in body
# e.g.
# INPUT: check_items("- [ ] item1\n- [ ] item2\n- [ ] item3", c("- [ ] item2", "- [ ] item3"))
# OUTPUT: "- [ ] item1\n- [x] item2\n- [x] item3"
check_items <- function(body, items_to_check) {
  # split body into items
  all_items <- get_items(body)
  # get the intersection
  intersection <- intersect(all_items, items_to_check)
  # subtract items_to_check from all_items
  remaining_items <- setdiff(all_items, items_to_check)
  # get checked_items
  checked_items <- sapply(items_to_check, check_item)
  # union items_to_check_with checked_items
  updated_items <- union(remaining_items, checked_items)
  # return updated body
  paste(updated_items, collapse = "\n")
}


uncheck_items <- function(body, items_to_uncheck) {
  # split body into items
  all_items <- get_items(body)
  # get the intersection
  intersection <- intersect(all_items, items_to_uncheck)
  # subtract items_to_check from all_items
  remaining_items <- setdiff(all_items, items_to_uncheck)
  # get checked_items
  unchecked_items <- sapply(items_to_uncheck, uncheck_item)
  # union items_to_check_with checked_items
  updated_items <- union(remaining_items, unchecked_items)
  # return updated body
  paste(updated_items, collapse = "\n")
}

# gives a list of items in body
get_items <- function(body) {
  # get lines of body
  lines <- strsplit(body, "\n")[[1]]
  # get checked/unchecked items (exclude other lines of text)
  # starts with - [ ] or - [x]
  lines[stringr::str_detect(lines, "^- \\[ \\]|- \\[x\\]")]
}

update_body <- function(body, items_to_check, items_to_uncheck) {
  # check items
  checked_body <- check_items(body, items_to_check)
  # uncheck items
  checked_and_unchecked_body <- uncheck_items(checked_body, items_to_uncheck)
  # return updated body
  checked_and_unchecked_body
}


# create_issue, create_issues, and create_checklists return issue numbers that can be input to update_issue
#
# example input:
# update_issue("A2-Ai", "test-qc-api", 65, c("- [ ] are units in diganostic figures correct? "),
# c("- [x] does the output match what the comments expect? "))
# output body:
# "- [ ] does the seed match from when the script was last run? \n- [x] are units in diganostic figures correct? \n- [ ] does the output match what the comments expect? "
update_issue <- function(owner, repo, issue_number, items_to_check = c(), items_to_uncheck = c()) {
  # get issue
  issue <- gh::gh("GET /repos/:owner/:repo/issues/:issue_number", .api_url = .le$github_api_url,
                  owner = owner,
                  repo = repo,
                  issue_number = issue_number)

  # update body
  updated_body <- update_body(issue$body, items_to_check, items_to_uncheck)

  # update issue
  gh::gh("PATCH /repos/:owner/:repo/issues/:issue_number",
                          owner = owner,
                          repo = repo,
                          issue_number = issue_number,
                          body = updated_body)
}
