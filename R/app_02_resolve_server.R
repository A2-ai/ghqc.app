#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom dplyr %>%
#' @importFrom log4r warn error info debug
#' @importFrom purrr map_df
#' @importFrom gert git_status git_ahead_behind
NULL

ghqc_resolve_server <- function(id, remote, org, repo, milestone_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    preview_trigger <- reactiveVal(FALSE)
    post_trigger <- reactiveVal(FALSE)

    observe({
      req(remote)
      waiter_hide()
    })

    observe({
      req(milestone_list)

      updateSelectInput(
        session,
        "select_milestone",
        choices = c("All Issues", milestone_list)
      )
    })

    issue_choices <- reactive({
      req(input$select_milestone)

      w_gh <- create_waiter(ns, sprintf("Fetching Issue data for %s ...", input$select_milestone))
      w_gh$show()
      on.exit(w_gh$hide())

      tryCatch(
        {
          if (input$select_milestone == "All Issues") {
            all_issues <- get_all_issues_in_repo(owner = org, repo = repo)
            issue_choices <- convert_issue_df_format(all_issues)
          } else {
            issues_by_milestone <- get_all_issues_in_milestone(owner = org, repo = repo, milestone_name = input$select_milestone)
            issue_choices <- convert_issue_df_format(issues_by_milestone)
          }
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving issues: {e$message}"))
          rlang::abort(e$message)
        }
      )
    })

    observe({
      req(issue_choices())

      updateSelectInput(
        session,
        "select_issue",
        choices = issue_choices()
      )
    })

    issue_parts <- reactive({
      req(input$select_issue)
      split_issue_parts(input$select_issue)
    })

    all_commits <- reactive({
      req(org, repo, issue_parts()$issue_number)
      get_commits_df(
        issue_number = issue_parts()$issue_number,
        owner = org,
        repo = repo
      )
    })

    ref_commits <- reactive({
      req(all_commits())
      tryCatch(
        {
          ref_commits <- get_reference_df(
            commits_df = all_commits()
          )
          ref_commits <- convert_commits_df_format(ref_commits)
        },
        error = function(e) {
          debug(.le$logger, glue::glue("There was 0 reference commits for issue {issue_parts()$issue_number}: {e$message}"))
          NULL
        }
      )
    })

    observe({
      if (length(ref_commits()) == 0) {
        return(updateSelectizeInput(session, "ref_commits",
          choices = "", # needs "" as NULL doesn't give back placeholder
          options = list(
            placeholder = "No commits since QC initialization."
          )
        ))
      }

      # checks above all
      updateSelectizeInput(session, "ref_commits", choices = ref_commits())
    })

    comp_commits <- reactive({
      req(all_commits())
      req(input$ref_commits)

      tryCatch(
        {
          comp_commits <- get_comparator_df(
            commits_df = all_commits(),
            selected_reference_commit = input$ref_commits
          )

          comp_commits <- convert_commits_df_format(comp_commits)
        },
        error = function(e) {
          debug(.le$logger, glue::glue("There was 0 comparator commits for issue {issue_parts()$issue_number}: {e$message}"))
          NULL
        }
      )
    })

    observe({
      if (!isTruthy(comp_commits())) {
        return(updateSelectizeInput(session, "comp_commits",
          choices = "", # needs "" as NULL doesn't give back placeholder
          options = list(
            placeholder = "No commits since reference commit."
          )
        ))
      }
      updateSelectizeInput(session, "comp_commits", choices = comp_commits())
    })

    # https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
    modal_check <- eventReactive(c(input$preview, input$post), {
      tryCatch(
        {
          req(issue_parts()$issue_title)
          uncommitted_git_files <- git_status()$file
          git_sync_status <- git_ahead_behind()
          untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), issue_parts()$issue_title)
          commit_update_status <- check_if_updates_since_init(all_commits())
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving one of the status_checks items: {e$message}"))
        }
      )

      determine_modal_message(
        selected_files = issue_parts()$issue_title,
        uncommitted_git_files = uncommitted_git_files,
        untracked_selected_files = untracked_selected_files,
        git_sync_status = git_sync_status,
        commit_update_status = commit_update_status
      )
    })

    observeEvent(input$preview, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed_preview"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(modal_check()$message),
          footer = NULL,
          easyClose = TRUE
        ))
      } else {
        preview_trigger(TRUE)
      }
    })

    observeEvent(input$post, {
      req(modal_check())

      if (!is.null(modal_check()$message)) {
        showModal(modalDialog(
          title = tags$div(tagList(
            if (modal_check()$state == "warning") {
              actionButton(ns("proceed_post"), "Proceed Anyway")
            },
            actionButton(ns("return"), "Return")
          ), style = "text-align: right;"),
          HTML(modal_check()$message),
          footer = NULL,
          easyClose = TRUE
        ))
      } else {
        post_trigger(TRUE)
      }
    })

    preview_comment <- reactive({
      req(preview_trigger())
      preview_trigger(FALSE)
      tryCatch(
        {
          commits_for_compare <- case_when(
            input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
            input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
          )

          comment_body <- create_comment_body(org,
                                              repo,
                                              message = input$message,
                                              issue_number = issue_parts()$issue_number,
                                              diff = input$show_diff,
                                              comparator_commit = commits_for_compare$comparator_commit,
                                              reference_commit = commits_for_compare$reference_commit
          )
          html_file_path <- create_gfm_file(comment_body)
          custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")
        },
        error = function(e) {
          log_string <- glue::glue(
            "There was an error creating preview comment for Issue {issue_parts()$issue_number} in repository {org}/{repo}.\n",
            "Input Parameters:\n",
            "- Message: {input$message}\n",
            "- Show Diff: {input$show_diff}\n",
            "- Compare Type: {input$compare}\n",
            "- Comparator Commit: {commits_for_compare$comparator_commit}\n",
            "- Reference Commit: {commits_for_compare$reference_commit}\n",
            "Error Message: {e$message}"
          )
          error(.le$logger, log_string)
          rlang::abort(e$message)
        }
      )
    })

    observe({
      # req preview_comment causes modal not to show
      showModal(modalDialog(
        title = tags$div(
          style = "display: flex;
          justify-content: space-between;
          align-items: center;",
          "Comment Preview",
          modalButton("Dismiss")
        ),
        footer = NULL,
        easyClose = TRUE,
        HTML(preview_comment())
      ))
    })

    post_comment <- reactive({
      req(post_trigger())
      post_trigger(FALSE)

      tryCatch(
        {
          commits_for_compare <- case_when(
            input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
            input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
          )

          add_fix_comment(
            org,
            repo,
            message = input$message,
            issue_number = issue_parts()$issue_number,
            diff = input$show_diff,
            reference_commit = commits_for_compare$reference_commit,
            comparator_commit = commits_for_compare$comparator_commit
          )

          issue <- get_issue(org, repo, issue_parts()$issue_number)
          issue_url <- issue$html_url
        },
        error = function(e) {
          log_string <- glue::glue(
            "There was an error creating comment for issue {issue_parts()$issue_number} in repository {org}/{repo}.\n",
            "Input Parameters:\n",
            "- Message: {input$message}\n",
            "- Show Diff: {input$show_diff}\n",
            "- Compare Type: {input$compare}\n",
            "- Current Commit: {commits_for_compare$comparator_commit}\n",
            "- Previous Commit: {commits_for_compare$reference_commit}\n",
            "Error Message: {e$message}"
          )
          error(.le$logger, log_string)
          rlang::abort(e$message)
        }
      )
    })

    observe({
      # req post_comment causes modal not to show
      showModal(modalDialog(
        title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
        footer = NULL,
        easyClose = TRUE,
        tags$p("Resolved QC finding(s) commented successfully."),
        tags$a(href = post_comment(), "Click here to visit the updated Issue on Github", target = "_blank")
      ))
    })

    observe({
      debug(.le$logger, glue::glue("comment buttons are inactivated."))
      removeClass("preview", "enabled-btn")
      addClass("preview", "disabled-btn")

      removeClass("post", "enabled-btn")
      addClass("post", "disabled-btn")

      if (isTruthy(input$select_issue)) {
        debug(.le$logger, glue::glue("comment buttons are activated because there is an Issue selected: {input$select_issue}"))

        removeClass("preview", "disabled-btn")
        addClass("preview", "enabled-btn")

        removeClass("post", "disabled-btn")
        addClass("post", "enabled-btn")
      }
    })

    observeEvent(input$proceed_preview, {
      debug(.le$logger, glue::glue("preview comment button proceeded and modal removed."))
      removeModal()
      preview_trigger(TRUE)
    })

    observeEvent(input$proceed_post, {
      debug(.le$logger, glue::glue("post comment button proceeded and modal removed."))
      removeModal()
      post_trigger(TRUE)
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Comment button returned and modal removed."))
      removeModal()
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })

    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      session$reload()
    })

    return(input)
  })
}
