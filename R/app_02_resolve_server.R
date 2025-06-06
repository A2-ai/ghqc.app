#' @import shiny
#' @importFrom shinyjs enable disable addClass removeClass
#' @importFrom dplyr %>%
#' @importFrom log4r warn error info debug
#' @importFrom purrr map_df
#' @importFrom gert git_status
NULL

ghqc_notify_server <- function(id, open_milestone_names) {
  moduleServer(id, function(input, output, session) {

    # This section ensures that when an error occurs, the app stops
    # When an error occurs, the session ends. The other instance of this is when
    # the user clicks reset.
    # The logic here prevents the app from stopping when reset is clicked
    reset_triggered <- reactiveVal(FALSE)
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    ns <- session$ns
    preview_trigger <- reactiveVal(FALSE)
    post_trigger <- reactiveVal(FALSE)

    observe({
      waiter_hide()
    })

    observe({
      req(open_milestone_names)

      updateSelectInput(
        session,
        "select_milestone",
        choices = c("All Issues", open_milestone_names)
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
            all_issues <- get_all_issues_in_repo()
            issue_choices <- convert_issue_df_format(all_issues)
          } else {
            milestone <- get_milestone_from_name(input$select_milestone)
            issues_by_milestone <- get_all_issues_in_milestone_from_milestone_number(milestone_name = milestone$title,
                                                                                     milestone_number = milestone$number
                                                                                     )
            issue_choices <- convert_issue_df_format(issues_by_milestone)
          }

          # filter for only Issues in your branch
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving issues: {conditionMessage(e)}"))
          rlang::abort(conditionMessage(e))
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
      req(issue_parts()$issue_number)

      get_commits_df(issue_number = issue_parts()$issue_number)
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
          debug(.le$logger, glue::glue("There was 0 reference commits for issue {issue_parts()$issue_number}: {conditionMessage(e)}"))
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
          debug(.le$logger, glue::glue("There was 0 comparator commits for issue {issue_parts()$issue_number}: {conditionMessage(e)}"))
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
          git_sync_status <- check_ahead_behind()
          untracked_selected_files <- Filter(function(file) check_if_qc_file_untracked(file), issue_parts()$issue_title)
          commit_update_status <- check_if_updates_since_init(all_commits())
        },
        error = function(e) {
          error(.le$logger, glue::glue("There was an error retrieving one of the status_checks items: {conditionMessage(e)}"))
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

        if (modal_check()$state == "warning") {
          showModal(modalDialog(
            title = tags$div(
              tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("proceed_preview"), "Proceed Anyway"),
              actionButton(ns("return"), "Return"),
              style = "text-align: right;"
            ),
            HTML(modal_check()$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }
        else if (modal_check()$state == "error") {
          showModal(modalDialog(
            title = tags$div(
              tags$span("Error", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("return"), "Return"),
              style = "text-align: right;"
            ),
            HTML(modal_check()$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }
      }
      else {
        preview_trigger(TRUE)
      }
    })

    observeEvent(input$post, {
      req(modal_check())
      if (!is.null(modal_check()$message)) {
        if (modal_check()$state == "warning") {
          showModal(modalDialog(
            title = tags$div(
                  tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
                  actionButton(ns("proceed_preview"), "Proceed Anyway"),
                  actionButton(ns("return"), "Return"),
                  style = "text-align: right;"
                  ),
            HTML(modal_check()$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }

        else if (modal_check()$state == "error") {
          showModal(modalDialog(
            title = tags$div(
              tags$span("Error", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("return"), "Return"),
              style = "text-align: right;"
            ),
            HTML(modal_check()$message),
            footer = NULL,
            easyClose = TRUE
          ))
        }
      }
      else {
        #post_trigger(TRUE)
        preview_trigger(TRUE)
      }
    })

    comment_body_string <- reactive({
      tryCatch(
        {
          commits_for_compare <- case_when(
            input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
            input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
          )

          issue <- get_issue(issue_parts()$issue_number)
          comment_body_parts <- create_notify_comment_body(issue = issue,
                                                           message = input$message,
                                                           diff = input$show_diff,
                                                           comparator_commit = commits_for_compare$comparator_commit,
                                                           reference_commit = commits_for_compare$reference_commit
          )
          comment_body <- glue::glue_collapse(comment_body_parts)
        },
        error = function(e) {
          log_string <- glue::glue(
            "There was an error creating preview comment for Issue {issue_parts()$issue_number} in repository {.le$org}/{.le$repo}.\n",
            "Input Parameters:\n",
            "- Message: {input$message}\n",
            "- Show Diff: {input$show_diff}\n",
            "- Compare Type: {input$compare}\n",
            "- Comparator Commit: {commits_for_compare$comparator_commit}\n",
            "- Reference Commit: {commits_for_compare$reference_commit}\n",
            "Error Message: {conditionMessage(e)}"
          )
          error(.le$logger, log_string)
          rlang::abort(conditionMessage(e))
        }
      )
    })

    preview_comment <- reactive({
      req(preview_trigger())
      req(comment_body_string())
      preview_trigger(FALSE)

      commits_for_compare <- case_when(
        input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
        input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
      )
      tryCatch(
        {
          html_file_path <- create_gfm_file(comment_body_string())
          custom_html <- readLines(html_file_path, warn = FALSE) %>% paste(collapse = "\n")
        },
        error = function(e) {
          log_string <- glue::glue(
            "There was an error creating preview comment for Issue {issue_parts()$issue_number} in repository {.le$org}/{.le$repo}.\n",
            "Input Parameters:\n",
            "- Message: {input$message}\n",
            "- Show Diff: {input$show_diff}\n",
            "- Compare Type: {input$compare}\n",
            "- Comparator Commit: {commits_for_compare$comparator_commit}\n",
            "- Reference Commit: {commits_for_compare$reference_commit}\n",
            "Error Message: {conditionMessage(e)}"
          )
          error(.le$logger, log_string)
          rlang::abort(conditionMessage(e))
        }
      )
    })

    observe({
      # req preview_comment causes modal not to show
      req(preview_comment)
      showModal(modalDialog(
        title = tags$div(
          tags$span("Preview", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
          actionButton(ns("return"), "Cancel", style = "color: red;"),
          actionButton(ns("proceed_post"), "Post"),
          style = "text-align: right;"
        ),
        footer = NULL,
        easyClose = TRUE,
        HTML(preview_comment())
      ))
    })

    post_notify_comment <- reactive({
      req(post_trigger())
      req(comment_body_string())
      post_trigger(FALSE)

      w_pc <- create_waiter(ns, "Posting QC notification...")
      w_pc$show()
      on.exit(w_pc$hide())

      commits_for_compare <- case_when(
        input$compare == "init" ~ list(comparator_commit = "current", reference_commit = "original"),
        input$compare == "comparators" ~ list(comparator_commit = input$comp_commits, reference_commit = input$ref_commits)
      )

      tryCatch(
        {
          post_comment(issue_number = issue_parts()$issue_number,
                       body = comment_body_string())

          issue <- get_issue(issue_parts()$issue_number)
          issue_url <- issue$html_url
        },
        error = function(e) {
          log_string <- glue::glue(
            "There was an error creating comment for issue {issue_parts()$issue_number} in repository {.le$org}/{.le$repo}.\n",
            "Input Parameters:\n",
            "- Message: {input$message}\n",
            "- Show Diff: {input$show_diff}\n",
            "- Compare Type: {input$compare}\n",
            "- Current Commit: {commits_for_compare$comparator_commit}\n",
            "- Previous Commit: {commits_for_compare$reference_commit}\n",
            "Error Message: {conditionMessage(e)}"
          )
          error(.le$logger, log_string)
          rlang::abort(conditionMessage(e))
        }
      )
    })

    observe({
      # req post_comment causes modal not to show
      showModal(modalDialog(
        title = tags$div(
          tags$span("QC notification posted", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
          modalButton("Dismiss"),
          style = "text-align: right;"
          ),
        footer = NULL,
        easyClose = TRUE,
        tags$a(href = post_notify_comment(), "Click here to view the Issue on GitHub", target = "_blank")
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
        is_binary <- stringr::str_detect(input$select_issue, exclude_patterns())
        shinyjs::toggle(id = "show_diff_wrap", condition = !is_binary)


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
      reset_triggered(TRUE)
      session$reload()
    })

    return(input)
  })
}
