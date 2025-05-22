#' @import shiny
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom glue glue
#' @importFrom log4r warn error info debug
#' @importFrom shinyjs enable disable addClass removeClass delay
#' @importFrom waiter Waiter spin_1 spin_2 waiter_hide
#' @importFrom gert git_status
#' @importFrom rprojroot find_rstudio_root_file
NULL

ghqc_status_server <- function(id,
                               open_milestone_names,
                               open_milestone_objects,
                               default_milestones,
                               local_commits,
                               remote_commits,
                               current_branch,
                               ahead_behind_status,
                               files_changed_in_remote_commits,
                               files_changed_in_unpushed_local_commits,
                               files_with_uncommitted_local_changes) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # This section ensures that when an error occurs, the app stops
    # When an error occurs, the session ends. The other instance of this is when
    # the user clicks reset.
    # The logic here prevents the app from stopping when reset is clicked
    session$onSessionEnded(function() {
      if (!isTRUE(isolate(reset_triggered()))) {
        stopApp()
      }
    })

    w <- waiter::Waiter$new(
      id = ns("main_panel_dynamic"),
      html = tagList(waiter::spin_1(), h4("Generating table...")),
      color = "rgba(0,0,0,0.5)"
    )

    # reactives
    reset_triggered <- reactiveVal(FALSE)
    show_table <- reactiveVal(FALSE)
    cached_status <- reactiveVal(NULL)
    last_milestones <- reactiveVal(NULL)
    status_cache <- reactiveVal(list())
    non_qc_repo_cache <- reactiveVal(list())
    prev_directories <- reactiveVal(NULL)

    # variable reactives
    current_branch_rv <- reactiveVal(current_branch)
    local_commits_rv <- reactiveVal(local_commits)
    remote_commits_rv <- reactiveVal(remote_commits)
    ahead_behind_status_rv <- reactiveVal(ahead_behind_status)
    files_changed_in_remote_commits_rv <- reactiveVal(files_changed_in_remote_commits)
    files_changed_in_unpushed_local_commits_rv <- reactiveVal(files_changed_in_unpushed_local_commits)
    files_with_uncommitted_local_changes_rv <- reactiveVal(files_with_uncommitted_local_changes)

    # if "Show closed milestones" is selected, calulate these
    closed_milestone_objects_rv <- reactiveVal(NULL)
    all_milestone_objects_rv <- reactiveVal(NULL)
    all_milestone_names_rv <- reactiveVal(NULL)

    # comment reactives
    post_notification_trigger <- reactiveVal(FALSE)
    post_approve_trigger <- reactiveVal(FALSE)
    post_unapprove_trigger <- reactiveVal(FALSE)


    # cache previously rendered sets of milestones
    milestone_key <- function(milestones) {
      paste(sort(milestones), collapse = "|")
    }

    repo_cache_key <- function(milestones, directories) {
      paste(
        paste(sort(milestones), collapse = "|"),
        paste(sort(directories), collapse = "|"),
        sep = "::"
      )
    }

    # if there's no cache, wait 1.5 seconds to make sure user if actually done selecting multiple milestones
    selected_milestones <- reactive({
      current <- selected_raw()
      key <- milestone_key(current)

      if (key %in% names(status_cache())) {
        current
      } else {
        selected_debounced()
      }
    })

    selected_raw <- reactive({
      input$selected_milestones
    })

    selected_debounced <- selected_raw %>% debounce(300)











    # ghqc_status
    run_generate <- function(milestones = selected_milestones()) {
      req(milestones)
      if (length(milestones) == 0 || is.null(milestones)) return(NULL)

      current_milestones <- milestones
      cache <- status_cache()

      missing <- setdiff(current_milestones, names(cache))

      # if any milestones not in cache, re-run ghqc_status
      if (length(missing) > 0) {
        w$show()
        for (milestone_name in missing) {
          debug(.le$logger, glue("Fetching statuses for uncached Milestone: {milestone_name}"))
          milestone_object <- get_milestone_object_from_milestone_name(milestone_name, append(open_milestone_objects, closed_milestone_objects_rv()))

          result <- ghqc_status(
            milestone_objects = list(milestone_object),
            current_branch_rv(),
            local_commits_rv(),
            remote_commits_rv(),
            ahead_behind_status_rv(),
            files_changed_in_remote_commits_rv(),
            files_changed_in_unpushed_local_commits_rv(),
            files_with_uncommitted_local_changes_rv()
          )

          relevant_df <- create_relevant_files_df(
            all_relevant_files = result$relevant_files,
            local_commits = local_commits_rv(),
            remote_commits = remote_commits_rv(),
            ahead_behind_status = ahead_behind_status_rv(),
            files_changed_in_remote_commits = files_changed_in_remote_commits_rv(),
            files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits_rv(),
            files_with_uncommitted_local_changes = files_with_uncommitted_local_changes_rv()
          )

          cache[[milestone_name]] <- list(
            status = result$status,
            relevant_files = result$relevant_files,
            relevant_files_df = relevant_df,
            issue_objects = result$issue_objects,
            repo_files = NULL
          )
        } # for

        status_cache(cache)
        w$hide()
      }

      combined_status <- do.call(rbind, lapply(cache[milestones], `[[`, "status"))
      cached_status(combined_status)

      last_milestones(current_milestones)
      show_table(TRUE)
      waiter_hide()
    } # run_generate


    get_combined_status_df <- function(milestones,
                                       status_cache,
                                       non_qc_repo_cache,
                                       show_relevant = TRUE,
                                       show_repo = TRUE,
                                       selected_dirs,
                                       local_commits,
                                       remote_commits,
                                       ahead_behind_status,
                                       files_changed_in_remote_commits,
                                       files_changed_in_unpushed_local_commits,
                                       files_with_uncommitted_local_changes) {
      # Start with cached status
      df <- do.call(rbind, lapply(status_cache[milestones], `[[`, "status"))

      # Add relevant files
      if (isTRUE(show_relevant)) {
        relevant_df <- do.call(rbind, lapply(status_cache[milestones], function(x) x$relevant_files_df))
        df <- rbind(df, relevant_df)
      }

      # Add non-QC repo files
      if (isTRUE(show_repo)) {
        key <- repo_cache_key(milestones, selected_dirs)

        if (!key %in% names(non_qc_repo_cache)) {
          all_relevant_files <- do.call(rbind, lapply(status_cache[milestones], `[[`, "relevant_files"))
          files_with_issues <- df$file_name

          repo_df <- create_non_issue_repo_files_df(
            files_with_issues = files_with_issues,
            local_commits = local_commits,
            remote_commits = remote_commits,
            all_relevant_files = all_relevant_files,
            selected_dirs = selected_dirs,
            ahead_behind_status = ahead_behind_status,
            files_changed_in_remote_commits = files_changed_in_remote_commits,
            files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits,
            files_with_uncommitted_local_changes = files_with_uncommitted_local_changes
          )

          non_qc_repo_cache[[key]] <- repo_df
        }

        df <- rbind(df, non_qc_repo_cache[[key]])
      }

      df
    }

    # generate table with default milestones when app is first loaded
    observeEvent(selected_milestones(), {
      if (is.null(cached_status()) && length(selected_milestones()) > 0) {
        run_generate()
      }
    }, once = TRUE)

    observeEvent(selected_milestones(), {
      if (length(selected_milestones()) > 0) {
        run_generate()
      }
    })

    # filter data based on filters in sidebar
    filtered_data <- reactive({
      req(show_table())
      req(cached_status())
      req(input$qc_status_filter)

      df <- combined_status_with_repo_files()

      # QC Status Filter
      if (input$qc_status_filter == "On track") {
        on_track <- c(
          "Awaiting approval",
          "File changes to post",
          "Approved"
        )
        df <- df[df$`QC Status` %in% on_track, ]
      }
      else if (input$qc_status_filter == "Needs attention") {
        needs_attention <- c(
          "Notification posted",
          "Approved; subsequent file changes",
          "Notification posted",
          "File changes to pull",
          "Initial QC commit posted",
          "Issue re-opened after approval",
          "Closed without approval",
          "Error",
          "QC branch deleted before approval"
          )

        df <- df[df$`QC Status` %in% needs_attention, ]

      }

      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        df <- df[dirname(df$file_name) %in% input$file_directory_filter, ]
      }

      df
    })

    relevant_files <- reactive({
      req(last_milestones())
      cache <- status_cache()
      do.call(rbind, lapply(cache[last_milestones()], `[[`, "relevant_files"))
    })

    base_file_list <- reactive({
      get_combined_status_df(
        milestones = last_milestones(),
        status_cache = status_cache(),
        non_qc_repo_cache = non_qc_repo_cache(),
        show_relevant = isTruthy(input$show_relevant_files),
        show_repo = isTruthy(input$show_repo_files),
        selected_dirs = input$file_directory_filter,
        local_commits = local_commits_rv(),
        remote_commits = remote_commits_rv(),
        ahead_behind_status = ahead_behind_status_rv(),
        files_changed_in_remote_commits = files_changed_in_remote_commits_rv(),
        files_changed_in_unpushed_local_commits = files_changed_in_unpushed_local_commits_rv(),
        files_with_uncommitted_local_changes = files_with_uncommitted_local_changes_rv()
      )
    })

    combined_status_with_repo_files <- reactive({
      debug(.le$logger, "combined_status_with_repo_files() triggered")
      input$show_repo_files

      df <- base_file_list()

      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        df <- df[dirname(df$file_name) %in% input$file_directory_filter, ]
      }

      df
    })

    # get a list of the directories of QC files
    file_directories <- reactive({
      debug(.le$logger, "file_directories() triggered")
      df <- base_file_list()
      files <- df$file_name

      if (is.null(files) || length(files) == 0 || !is.character(files)) {
        return(character(0))
      }

      unique(dirname(files))
    })

    observeEvent(file_directories(), {
      debug(.le$logger, "observeEvent(file_directories()) triggered")
      new_dirs <- file_directories()
      old_dirs <- prev_directories()

      if (!identical(sort(new_dirs), sort(old_dirs))) {
        prev_directories(new_dirs)

        shinyjs::delay(200, {
          updateSelectizeInput(
            session,
            inputId = "file_directory_filter",
            choices = new_dirs,
            selected = isolate(input$file_directory_filter),
            label = "File Directory Filter",
            options = list(
              placeholder = "(Optional)"
            )
          )
        })
      }
    }) # file_directory_filter




    observeEvent(input$show_closed, {
      if (isTRUE(input$show_closed)) {
        all_milestone_names <- all_milestone_names_rv()

        if (is.null(all_milestone_names)) {
          w_closed <- waiter::Waiter$new(
            id = ns("main_container"),
            html = tagList(waiter::spin_1(), h4("Retrieving closed Milestones...")),
            color = "rgba(0,0,0,0.5)"
          )
          w_closed$show()

          closed_milestone_objects <- get_closed_non_empty_milestone_objects()
          closed_milestone_objects_rv(closed_milestone_objects)

          closed_milestone_names <- get_milestone_names_from_milestone_objects(closed_milestone_objects)

          all_milestone_objects <- c(open_milestone_objects, closed_milestone_objects)
          all_by_branch <- group_milestone_objects_by_branch(all_milestone_objects)
          all_milestone_objects_rv(all_by_branch)

          all_milestone_names <- get_grouped_milestone_names(all_by_branch, closed_milestone_names)
          all_milestone_names_rv(all_milestone_names)

          w_closed$hide()
        }

        selected <- intersect(input$selected_milestones, unlist(all_milestone_names))
        placeholder <- ifelse(length(all_milestone_names) == 0, "No Milestones", "Select Milestone(s)")

        updateSelectizeInput(
          session,
          "selected_milestones",
          choices = all_milestone_names,
          selected = selected,
          options = list(
            render = I("
  {
    item: function(item, escape) {
      return '<div class=\"item\">' + item.label + '</div>';
    },
    option: function(item, escape) {
      return '<div class=\"option\">' + item.label + '</div>';
    }
  }
"),
            placeholder = placeholder
          )
        )
      }
      else {
        # show open-only
        selected <- intersect(input$selected_milestones, unlist(open_milestone_names))
        placeholder <- ifelse(length(open_milestone_names) == 0, "No open Milestones", "Select open Milestone(s)")

        choices <- if (!is.null(open_milestone_names)) {
          open_milestone_names
        }
        else {
          ""
        }

        updateSelectizeInput(
          session,
          "selected_milestones",
          choices = choices,
          selected = selected,
          options = list(
            placeholder = placeholder
          )
        )
      }
    })



    # QC APPROVED

    # when the green approve button is clicked
    observeEvent(input$show_approve_modal_row, {
      row_index <- input$show_approve_modal_row$row
      df <- filtered_data()
      req(nrow(df) >= row_index)

      file_name <- df[row_index, ]$file_name
      cache <- status_cache()
      milestone <- df[row_index, ]$milestone_name
      issue_name <- df[row_index, ]$file_name
      issue <- cache[[milestone]]$issue_objects[[milestone]][[issue_name]]
      warnings <- approve_warnings(issue = issue,
                                   row = df[row_index, ]
                                   )

      if (!is.null(warnings)) {
        showModal(modalDialog(
          title = tags$div(
            tags$span("Warning", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
            actionButton(ns("return"), "Cancel", style = "color: red;"),
            actionButton(ns("proceed_approve_preview"), "Proceed anyway"),
            style = "text-align: right;"
          ),
          footer = NULL,
          easyClose = TRUE,
          HTML(glue::glue("<p>The following warnings were detected for <b>{file_name}</b>:</p><ul>",
                          paste0("<li>", warnings, collapse = vspace()),
                          "</ul>"))
        ))
      } else {
        # No warnings — show approval preview modal immediately
        show_approve_preview_modal(df, row_index)
      }
    })

    show_approve_preview_modal <- function(df, row_index) {
      approve_comment_parts <- approve_comment_body()
      display_comment_body <- glue::glue_collapse(approve_comment_parts)
      path <- create_gfm_file(display_comment_body)
      html <- readLines(path, warn = FALSE) %>% paste(collapse = "\n")
      file_name <- df[row_index, ]$file_name

      showModal(modalDialog(
        title = tags$div(
          tags$span("Preview", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
          actionButton(ns("return"), "Cancel", style = "color: red;"),
          actionButton(ns("proceed_approve_post"), "Approve"),
          style = "text-align: right;"
        ),
        footer = NULL,
        easyClose = TRUE,
        tagList(
          HTML(glue::glue("Post this comment if no further feedback or file changes are pending, and you approve the final version of <b>{file_name}</b>.<br><br>")),
          textInput(ns("approve_message"), "Message", placeholder = "(Optional)"),
          HTML(html)
        )
      ))
    }

    approve_comment_body <- reactive({
      row_index <- input$show_approve_modal_row$row
      df <- filtered_data()
      req(df)

      cache <- status_cache()
      milestone <- df[row_index, ]$milestone_name
      issue_name <- df[row_index, ]$file_name
      issue <- cache[[milestone]]$issue_objects[[milestone]][[issue_name]]

      tryCatch({
        create_approve_comment_body(
          file_path = issue_name,
          initial_qc_commit = df[row_index, ]$initial_qc_commit,
          approved_qc_commit = df[row_index, ]$comparator_commit,
          issue = issue
        )
      }, error = function(e) {
        rlang::abort(conditionMessage(e))
      })
    })

    post_approve_comment <- observeEvent(post_approve_trigger(), {
      req(isTruthy(post_approve_trigger()))
      approve_comment_parts <- approve_comment_body()
      req(approve_comment_parts)
      row_index <- input$show_approve_modal_row$row
      df <- filtered_data()
      req(df)

      display_comment_body <- glue::glue_collapse(c(approve_comment_parts[1], input$approve_message, "\n\n", approve_comment_parts[2]))

      post_approve_trigger(FALSE)

      w_pc <- create_waiter(ns, "Approving QC...")
      w_pc$show()
      on.exit(w_pc$hide())

      tryCatch(
        {
          approve(issue_number = df[row_index, ]$issue_number,
                 body = display_comment_body
                 )

          showModal(modalDialog(
            title = tags$div(
              tags$span("QC approved", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("dismiss_modal"), "Dismiss"),
              style = "text-align: right;"
            ),
            footer = NULL,
            easyClose = TRUE,
            tags$a(href = df[row_index, ]$issue_url, "Click here to view the Issue on GitHub", target = "_blank")
          ))
        },
        error = function(e) {
          rlang::abort(conditionMessage(e))
        }
      )
    }) # post_approve_comment



    # QC NOTIFICATIONS

    observeEvent(input$show_notify_modal_row, {
      row_index <- input$show_notify_modal_row$row
      df <- filtered_data()
      req(nrow(df) >= row_index)
      comment_body_parts <- notify_comment_body()
      display_comment_body <- glue::glue_collapse(comment_body_parts)

      path <- create_gfm_file(display_comment_body)
      html <- readLines(path, warn = FALSE) %>% paste(collapse = "\n")

      file_name <- df[row_index, ]$file_name

      showModal(modalDialog(
        title = tags$div(
          tags$span("Preview", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
          actionButton(ns("return"), "Cancel", style = "color: red;"),
          actionButton(ns("proceed_notify_post"), "Notify"),
          style = "text-align: right;"
        ),
        footer = NULL,
        easyClose = TRUE,
        tagList(
          HTML(glue::glue("Post this comment to optionally notify collaborators about the most recent commit in Issue <b>{file_name}</b>.<br><br>")),
          textInput(ns("notify_message"), "Message", placeholder = "(Optional)"),
          HTML(html)
        )

      ))
    })

    notify_comment_body <- reactive({
      row_index <- input$show_notify_modal_row$row
      df <- filtered_data()
      req(df)

      cache <- status_cache()
      milestone <- df[row_index, ]$milestone_name
      issue_name <- df[row_index, ]$file_name
      issue <- cache[[milestone]]$issue_objects[[milestone]][[issue_name]]

      if (input$show_notify_modal_row$action == "Repost last QC notification") {
        comparator_commit <- df[row_index, ]$latest_qc_commit
        reference_commit <- df[row_index, ]$previous_qc_commit
      }
      else {
        comparator_commit <- df[row_index, ]$comparator_commit
        reference_commit <- df[row_index, ]$latest_qc_commit
      }

      tryCatch({
        create_notify_comment_body(
          issue = issue,
          message = NULL,
          diff = TRUE,
          comparator_commit = comparator_commit,
          reference_commit = reference_commit
        )
      }, error = function(e) {
        rlang::abort(conditionMessage(e))
      })
    })

    post_notify_comment <- observeEvent(post_notification_trigger(), {
      req(isTruthy(post_notification_trigger()))
      notify_comment_parts <- notify_comment_body()
      req(notify_comment_parts)
      row_index <- input$show_notify_modal_row$row
      df <- filtered_data()
      req(df)

      display_comment_body <- glue::glue_collapse(c(notify_comment_parts[1], input$notify_message, "\n\n", notify_comment_parts[2]))

      post_notification_trigger(FALSE)

      w_pc <- create_waiter(ns, "Posting QC notification...")
      w_pc$show()
      on.exit(w_pc$hide())

      tryCatch(
        {
          post_comment(issue_number = df[row_index, ]$issue_number,
                      body = display_comment_body)

          showModal(modalDialog(
            title = tags$div(
              tags$span("QC notification posted", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("dismiss_modal"), "Dismiss"),
              style = "text-align: right;"
            ),
            footer = NULL,
            easyClose = TRUE,
            tags$a(href = df[row_index, ]$issue_url, "Click here to view the Issue on GitHub", target = "_blank")
          ))
        },
        error = function(e) {
          rlang::abort(conditionMessage(e))
        }
      )
    }) # post_notify_comment


    ### UNAPPROVE
    # when the red button is clicked
    observeEvent(input$show_unapprove_modal_row, {
      row_index <- input$show_unapprove_modal_row$row
      df <- filtered_data()
      req(nrow(df) >= row_index)

      show_unapprove_preview_modal(df, row_index)
    })

    show_unapprove_preview_modal <- function(df, row_index) {
      unapprove_comment_parts <- unapprove_comment_body()
      display_comment_body <- glue::glue_collapse(unapprove_comment_parts)
      path <- create_gfm_file(display_comment_body)
      html <- readLines(path, warn = FALSE) %>% paste(collapse = "\n")
      file_name <- df[row_index, ]$file_name

      showModal(modalDialog(
        title = tags$div(
          tags$span("Preview", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
          actionButton(ns("return"), "Cancel", style = "color: red;"),
          actionButton(ns("proceed_unapprove_post"), "Unapprove"),
          style = "text-align: right;"
        ),
        footer = NULL,
        easyClose = TRUE,
        tagList(
          HTML(glue::glue("Post this comment to unapprove QC for <b>{file_name}</b>.<br><br>")),
          textInput(ns("unapprove_message"), "Message", placeholder = "Explain why QC is unapproved..."),
          HTML(html)
        )
      ))
    }

    unapprove_comment_body <- reactive({
      row_index <- input$show_unapprove_modal_row$row
      df <- filtered_data()
      req(df)

      cache <- status_cache()
      milestone <- df[row_index, ]$milestone_name
      issue_name <- df[row_index, ]$file_name
      issue <- cache[[milestone]]$issue_objects[[milestone]][[issue_name]]
      approve_comment <- df[row_index, ]$approve_comment

      tryCatch({
        create_unapprove_comment_body(
          approve_comment = approve_comment,
          issue = issue
        )
      }, error = function(e) {
        rlang::abort(conditionMessage(e))
      })
    })

    post_unapprove_comment <- observeEvent(post_unapprove_trigger(), {
      req(isTruthy(post_unapprove_trigger()))
      unapprove_comment_parts <- unapprove_comment_body()
      req(unapprove_comment_parts)
      row_index <- input$show_unapprove_modal_row$row
      df <- filtered_data()
      req(df)

      display_comment_body <- glue::glue_collapse(c(unapprove_comment_parts[1], input$unapprove_message, "\n\n", unapprove_comment_parts[2]))

      post_unapprove_trigger(FALSE)

      w_pc <- create_waiter(ns, "Unapproving QC...")
      w_pc$show()
      on.exit(w_pc$hide())

      tryCatch(
        {
          unapprove(issue_number = df[row_index, ]$issue_number,
                    unapprove_comment_body = display_comment_body,
                    approve_comment = df[row_index, ]$approve_comment
                    )

          showModal(modalDialog(
            title = tags$div(
              tags$span("QC Unapproved", style = "float: left; font-weight: bold; font-size: 20px; margin-top: 5px;"),
              actionButton(ns("dismiss_modal"), "Dismiss"),
              style = "text-align: right;"
            ),
            footer = NULL,
            easyClose = TRUE,
            tags$a(href = df[row_index, ]$issue_url, "Click here to view the Issue on GitHub", target = "_blank")
          ))
        },
        error = function(e) {
          rlang::abort(conditionMessage(e))
        }
      )
    }) # post_unapprove_comment







    ############ OUTPUT

    observe({ # only show "Show Non-QC Files" button when there are dirs selected in the file directory filter
      if (!is.null(input$file_directory_filter) && length(input$file_directory_filter) > 0) {
        shinyjs::show("show_repo_files_wrapper")
      }
      else {
        shinyjs::hide("show_repo_files_wrapper")
      }
    })

    output$sidebar <- renderUI({
      tagList(
        # Show closed Milestones
        div(
          style = "margin-top: 4px; margin-bottom: 5px",
          class = "form-group shiny-input-container",
          tags$label(
            style = "display: flex; align-items: center; justify-content: flex-start; gap: 8px; font-weight: 600; font-size: 13px; color: #333;",
            "Show closed Milestones",
            tags$input(
              id = ns("show_closed"),
              type = "checkbox",
              class = "form-check-input",
              style = "transform: scale(1.2)"
            )
          )
        ), # Show Closed Milestones
        # Milestones
        selectizeInput(ns("selected_milestones"),
                       "Milestones",
                       choices = open_milestone_names,
                       selected = default_milestones,
                       multiple = TRUE,
                       width = "100%"
                       ),
        # QC Status Filter
        selectInput(
          ns("qc_status_filter"),
          "QC Status Filter",
          choices = c("All", "On track", "Needs attention"),
          selected = "All",
          width = "100%"
        ),

        # File Directory Filter
        selectizeInput(
          ns("file_directory_filter"),
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        ),
        # Show non-QC repo files
        div(
          id = ns("show_repo_files_wrapper"),
          div(
            class = "form-group shiny-input-container",
            tags$label(
              style = "display: flex; align-items: center; justify-content: flex-start; gap: 8px; font-weight: 600; font-size: 13px; color: #333;",
              tags$span("Show non-QC files"),
              tags$input(
                id = ns("show_repo_files"),
                type = "checkbox",
                class = "form-check-input",
                style = "transform: scale(1.2) translateY(-2px);"
              )
            )
          )
        ), # Show non-QC repo files
        # Show relevant files
        div(
          class = "form-group shiny-input-container",
          tags$label(
            style = "display: flex; align-items: center; justify-content: flex-start; gap: 8px; font-weight: 600; font-size: 13px; color: #333;",
            tags$span("Show relevant files"),
            tags$input(
              id = ns("show_relevant_files"),
              type = "checkbox",
              class = "form-check-input",
              style = "transform: scale(1.2) translateY(-2px);",
              checked = "checked"
            )
          )
        ), # Show relevant files
        # Show QCer
        div(
          style = "margin-top: -6px;",
          class = "form-group shiny-input-container",
          tags$label(
            style = "display: flex; align-items: center; justify-content: flex-start; gap: 8px; font-weight: 600; font-size: 13px; color: #333;",
            "Show QCer",
            tags$input(
              id = ns("show_qcer"),
              type = "checkbox",
              class = "form-check-input",
              style = "transform: scale(1.2) translateY(-2px);"
            )
          )
        ) # Show QCer
      ) # tagList
    }) # output$sidebar


    milestones_initialized <- reactiveVal(FALSE)

    observeEvent(selected_milestones(), {
      if (!milestones_initialized() && length(selected_milestones()) >= 0) {
        milestones_initialized(TRUE)
      }
    }, ignoreInit = FALSE, once = TRUE)


    observeEvent(show_table(), {
      if (!isTRUE(milestones_initialized()) && length(open_milestone_objects) > 0) {
        return(NULL)
      }

      if (isTRUE(show_table()) && length(selected_milestones()) > 0) {
        output$main_panel_dynamic <- renderUI({
          div(
            id = ns("main_panel_wrapper"),
            DT::dataTableOutput(ns("status_table"))
          )
        })
      }
      else {
        waiter_hide()
        output$main_panel_dynamic <- renderUI({
          HTML("<div style='padding-top: 5px; font-size: small !important; font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif !important; color: #a94442; font-weight: 700;'>No Milestones selected</div>")
          })
      }
    })

    output$status_table <- DT::renderDataTable({
      debug(.le$logger, "status_table re-rendered")
      req(show_table())
      df <- filtered_data()

      if (nrow(df) > 0) {
        # Add action column
        df$Action <- sapply(1:nrow(df), function(i) {
          row_data <- df[i, ]$Action
          opts <- row_data$options %||% character(0)
          msg  <- row_data$message
          generate_action_ui(i, opts, msg)
        })
      } # if rows


      # remove info columns in display
      df <- df[, !colnames(df) %in% c("milestone_name",
                                      "file_name",
                                      "issue_number",
                                      "initial_qc_commit",
                                      "latest_qc_commit",
                                      "previous_qc_commit",
                                      "approve_comment",
                                      "comparator_commit",
                                      "issue_url")]


      # if only one milestone, don't need milestone column
      if (length(input$selected_milestones) == 1) {
        df <- df[, colnames(df) != "Milestone"]
      }

      if (!isTruthy(input$show_qcer)) {
        df <- df[, colnames(df) != "QCer", drop = FALSE]
      }

      pretty_table <- DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        class = "stripe hover compact display",
        filter = 'top',
        options = list(
          pageLength = -1, # shows all the rows
          lengthChange = FALSE,
          paging = FALSE,
          searching = TRUE,
          dom = 'it',
          language = list(
            info = ""
          ),
          scrollCollapse = TRUE,
          drawCallback = DT::JS(glue::glue("
  function(settings) {{
    var table = this.api();

    function fixHeaderAlignment() {{
      table.columns.adjust();
    }}

    // Slight delay to allow sidebar collapse animation to complete
    setTimeout(fixHeaderAlignment, 300);

    $(window).off('resize.dt').on('resize.dt', function() {{
      fixHeaderAlignment();
    }});

    // Observe sidebar if it exists
    const sidebar = document.querySelector('[id$=\"-sidebar\"]');
    if (sidebar && typeof ResizeObserver !== 'undefined') {{
      new ResizeObserver(() => {{
        setTimeout(fixHeaderAlignment, 300);
      }}).observe(sidebar);
    }}

    var info = table.page.info();
      var recordText = info.recordsDisplay === 1 ? 'file' : 'files';
      $('div.dataTables_info').text(
        `${{info.recordsDisplay}} ` + recordText
      );
  }}
"))
        )
      ) %>%
        # format Issue State column
        DT::formatStyle(
          "Issue State",
          color = DT::styleEqual(
            c("Open", "Closed"),
            c("black", "#27770a")
          )
        ) %>%
        # format QC Status column
        DT::formatStyle(
          "QC Status",
          color = DT::styleEqual(
            c("Approved",
              "Awaiting approval",
              "File changes to post",
              "Notification posted",
              "File changes to pull",
              "Initial QC commit posted",
              "Issue re-opened after approval",
              "Approved; subsequent file changes",
              "Closed without approval",
              "Error",
              "QC branch deleted before approval"
              ),
            c("#27770a", "black", "black", "#a94442", "#a94442", "#a94442", "#a94442", "#a94442", "#a94442", "#a94442", "#a94442"),
            default = "black"
          )
        ) %>%
        # format Git Status column
        DT::formatStyle(
          "Git Status",
          color = DT::styleEqual(
            c("Up to date",
              "Remote file changes",
              "File does not exist locally",
              "Local uncommitted file changes",
              "Local unpushed commits with file changes"
              ),
            c("#27770a", "#a94442", "#a94442", "#a94442", "#a94442"),
            default = "black"
          )
        ) %>%
        DT::formatStyle(
          c('File', 'Diagnostics'),
          `white-space` = 'normal',
          `word-wrap` = 'break-word',
          `max-width` = '200px'
        )

      pretty_table
    })

    observeEvent(input$close, {
      debug(.le$logger, glue::glue("App was closed through the close button."))
      stopApp()
    })


    observeEvent(input$proceed_notify_post, {
      debug(.le$logger, glue::glue("post comment button proceeded and modal removed."))
      removeModal()
      post_notification_trigger(TRUE)
    })

    observeEvent(input$proceed_approve_preview, {
      removeModal()
      df <- filtered_data()
      row_index <- input$show_approve_modal_row$row
      show_approve_preview_modal(df, row_index)
    })

    observeEvent(input$proceed_approve_post, {
      debug(.le$logger, glue::glue("post approve comment button proceeded and modal removed."))
      removeModal()
      post_approve_trigger(TRUE)
    })

    observeEvent(input$proceed_unapprove_preview, {
      removeModal()
      df <- filtered_data()
      row_index <- input$show_unapprove_modal_row$row
      show_unapprove_preview_modal(df, row_index)
    })

    observeEvent(input$proceed_unapprove_post, {
      debug(.le$logger, glue::glue("post unapprove comment button proceeded and modal removed."))
      removeModal()
      post_unapprove_trigger(TRUE)
    })

    observeEvent(input$return, {
      debug(.le$logger, glue::glue("Button returned and modal removed."))
      removeModal()
    })

    observeEvent(input$dismiss_modal, {
      debug(.le$logger, "Dismiss clicked - resetting app")
      removeModal()
      reset_app()
    }, ignoreInit = TRUE)

    observeEvent(input$reset, {
      debug(.le$logger, glue::glue("App was reset through the reset button."))
      reset_app()
    }) # input$reset

    reset_app <- function() {
      debug(.le$logger, glue::glue("App was reset."))

      current_milestones <- input$selected_milestones

      reset_triggered(TRUE)
      cached_status(NULL)
      last_milestones(NULL)
      status_cache(list())
      non_qc_repo_cache(list())

      show_table(FALSE)

      updateSelectizeInput(
        session,
        inputId = "selected_milestones",
        selected = current_milestones
      )

      # recompute reactiveVal variables
      current_branch_rv(gert::git_branch())
      local_commits_rv(get_local_commits())
      remote_commits_rv(get_remote_commits(current_branch_rv()))
      ahead_behind_status_rv(check_ahead_behind())
      files_changed_in_remote_commits_rv(get_files_changed_in_remote_commits(remote_commits_rv(), ahead_behind_status_rv()))
      files_changed_in_unpushed_local_commits_rv(get_files_changed_in_unpushed_local_commits(local_commits_rv(), ahead_behind_status_rv()))
      files_with_uncommitted_local_changes_rv(get_files_with_uncommitted_local_changes())

      show_table(FALSE)
      run_generate(current_milestones)
    } # reset_app

    return(input)
  }) # moduleServer
} # ghqc_status_server
