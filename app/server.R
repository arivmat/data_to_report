## Andrea Rivera Mateos

server <- function(input, output, session) {
  
  # Main reactives ----
  dataset <- reactiveVal(NULL)                          # dataset
  selectedVariables <- reactiveVal(NULL)                # selected varaibles
  values <- reactiveValues()                            # miscellaneous storage (e.g. project_data)
  analysis_storage <- reactiveVal(list())               # saved analysis results
  dashboard_items <- reactiveVal(list())                # dashboard box configurations
  dashboard_containers_list <- reactiveVal(list())
  dashboard_container_counter <- reactiveVal(0)
  
  
  # 1. Home----
  
  ## 1.1 new project ----
  
  ### form ----
  observeEvent(input$button1, {
    showModal(
      modalDialog(
        title = "Create new project",
        textInput("project_name", "Project name:"),      # project name
        textInput("client_name",  "Client name:"),       # client name
        fileInput(
          "datafile",
          "Upload Data File (.csv, .xlsx, .dta)",
          accept = c(".csv", ".xlsx", ".xls", ".dta")    # data file
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_button", "Save and Continue", class = "btn btn-primary")
        ),
        easyClose = TRUE,
        size = "m"
      )
    )
  })

  
  ### save & continue----
  observeEvent(input$save_button, {
    project_name <- input$project_name
    client_name  <- input$client_name
    datafile     <- input$datafile
    
    # file is selected
    if (is.null(datafile)) {
      showModal(modalDialog(
        title = "Error",
        "Please upload a valid data file (.csv, .xlsx, .dta)",
        easyClose = TRUE
      ))
      return()
    }
    
    # read data
    raw_data <- read_datafile(datafile)
    if (is.null(raw_data)) {
      return()
    }
    
    # clean data
    clean_data <- clean_dataset(raw_data)
    
    ## store clean dta into our reactives
    dataset(clean_data)
    selectedVariables(names(clean_data))
    values$project_file_name <- datafile$name
    
    # store minimal project info until full save
    values$project_data <- list(
      name   = input$project_name,
      client = input$client_name,
      date   = Sys.Date()
    )
    
    # clean prior analysis and dahbord_items
    analysis_storage(list())
    dashboard_items(list())
    
    # switch to Data Cleaning tab
    updateTabItems(session, "sidebar_tabs", "data_cleaning")
    
    # close modal
    removeModal()
  })
  
  
  ## 1.2 previous project ----
  
  ### list existing .rds files ----
  observeEvent(input$button2, {
    if (!dir.exists("saved_projects")) dir.create("saved_projects")
    files <- list.files("saved_projects", pattern = "\\.rds$", full.names = TRUE)
    
    # if no files, alert and return
    if (length(files) == 0) {
      showModal(modalDialog(
        title = "No Projects Found",
        "There are no saved projects to load.",
        easyClose = TRUE
      ))
      return()
    }
    
    
    # build a small data.frame with metadata to display
    project_list <- lapply(files, function(path) {
      info    <- file.info(path)
      proyecto <- readRDS(path)
      data.frame(
        Name         = proyecto$name,
        Client       = proyecto$client,
        DateModified = as.Date(info$mtime),
        stringsAsFactors = FALSE
      )
    })
    
    values$projects_df <- do.call(rbind, project_list)
    values$projects_df <- values$projects_df[order(values$projects_df$DateModified, 
                                                   decreasing = TRUE), ] # sort projects descending by modification date
    
    # show modal with existing previous project
    showModal(modalDialog(
      title = "Select an Existing Project",
      textInput("search_project", "Search by Name or Client:"),
      DTOutput("projects_table"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("preview_selected_project", "Preview Project")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  
  ### render projects table ----
  output$projects_table <- renderDT({
    req(values$projects_df)
    df <- values$projects_df
    search_term <- input$search_project
    if (!is.null(search_term) && search_term != "") {
      df <- df[grepl(search_term, df$Name, ignore.case = TRUE) |
                 grepl(search_term, df$Client, ignore.case = TRUE), ]
    }
    datatable(df, rownames = FALSE, selection = "single", options = list(pageLength = 5))
  })
  
  
  ### preview selected project ----
  observeEvent(input$preview_selected_project, {
    req(input$projects_table_rows_selected)
    row_sel      <- input$projects_table_rows_selected
    info_proyect <- values$projects_df[row_sel, ]
    ruta_rds     <- file.path("saved_projects", paste0(info_proyect$Name, ".rds"))
    
    if (!file.exists(ruta_rds)) {
      showModal(modalDialog(
        title = "Error",
        "The file for that project was not found. Please try again.",
        easyClose = TRUE
      ))
      return()
    }
    
    full_project <- readRDS(ruta_rds)
    values$project_data <- full_project
    
    # show data preview
    showModal(modalDialog(
      title = paste0("Data Preview: ", info_proyect$Name),
      DTOutput("project_data_table"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_data", "Update Data"),
        actionButton("continue_editing", "Continue Editing")
      ),
      size = "l",
      easyClose = TRUE
    ))
    
    # render preview data table
    output$project_data_table <- renderDT({
      datatable(full_project$data, options = list(pageLength = 5, scrollX = TRUE))
    })
  })
  
  
  ### update/continue ----
  
  ##### continue editing ----
  observeEvent(input$continue_editing, {
    removeModal()
    
    # restore reactives 
    load_project(
      project           = values$project_data,
      session           = session,
      dataset           = dataset,
      selectedVariables = selectedVariables,
      analysis_storage  = analysis_storage,
      dashboard_items_reactive = dashboard_items,
      input                     = input,
      output                    = output
    )
    
    # go to data cleaning tab
    updateTabItems(session, "sidebar_tabs", "dashboard_view")
  })
  
  
  #### update data ----
  observeEvent(input$update_data, {
    showModal(modalDialog(
      title = "Select a New Data File",
      fileInput("new_datafile", "Upload new file (.csv, .xlsx, .dta)", 
                accept = c(".csv", ".xlsx", ".xls", ".dta")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_upload", "Confirm Upload")
      ),
      size = "m"
    ))
  })
  
  # handle new data
  observeEvent(input$confirm_upload, {
    req(input$new_datafile)
    new_data_raw <- read_datafile(input$new_datafile)
    if (is.null(new_data_raw)) return()
    
    ## ensure column names match the original
    viejo_cols <- colnames(values$project_data$data)
    nuevo_cols <- colnames(new_data_raw)
    if (!all(viejo_cols == nuevo_cols)) {
      showModal(modalDialog(
        title = "Error",
        "The columns names do NOT match the original project.",
        easyClose = TRUE
      ))
      return()
    }
    
    ## clean new data
    new_data_clean <- clean_dataset(new_data_raw)
    pd <- values$project_data
    pd$data <- new_data_clean
    
    
    ## replace data in reactives and on disk
    dataset(new_data_clean)
    pd$analysis_list <- analysis_storage()
    pd$dashboard_items <- dashboard_items() 
    values$project_data <- pd
    
    saveRDS(pd, file.path("saved_projects", paste0(pd$name, ".rds")))
    
    load_project(
      project           = pd,
      session           = session,
      dataset           = dataset,
      selectedVariables = selectedVariables,
      analysis_storage  = analysis_storage,
      dashboard_items_reactive = dashboard_items,
      input                     = input,
      output                    = output
    )
    
    ## success updated data
    showModal(modalDialog(
      title = "Success!",
      "The data has been updated successfully.",
      easyClose = FALSE,
      footer = actionButton("ok_continue", "Continue Editing")
    ))
    
    ## go to dashboard tab
    updateTabItems(session, "sidebar_tabs", "dashboard_view")
    })
  
  ## close update data modal
  observeEvent(input$ok_continue, {
    removeModal()
    updateTabItems(session, "sidebar_tabs", "data_cleaning")
    })

  
  
  # 2. Data cleaning ----
  
  ## 2.1 synchronize variable‐selector checkboxGroup ----
  
  # whenever dataset changes, update the choices & selected values in the UI
  observeEvent(dataset(), {
    df <- dataset()
    if (!is.null(df)) {
      updateCheckboxGroupInput(
        session,
        "variables",                        # checkboxGroupInput in UI
        choices  = names(df),               # all column names become choices
        selected = selectedVariables()      # keep current selections
      )
    }
  })
  
  # deselect all variables button
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "variables", selected = character(0))
  })
  
  ## keep selected variables in sync with UI
  observeEvent(input$variables, {
    selectedVariables(input$variables)
  })
  

  ## 2.2 summary table ----
  output$data_summary_clean <- renderDT({
    req(dataset(), selectedVariables())
    df_all       <- dataset()
    sel_vars     <- selectedVariables()
   
    # if no variables selected, empty table
    if (length(sel_vars) == 0) {
      return(datatable(
        data.frame(Message = "No variables selected"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # subset to only the selected columns
    df <- df_all[, sel_vars, drop = FALSE]
    
    # compute metrics
    var_names   <- names(df)
    var_classes <- sapply(df, function(col) class(col)[1])
    n_unique    <- sapply(df, function(col) length(unique(col)))
    pct_miss    <- sapply(df, function(col) round(sum(is.na(col)) / length(col) * 100, 2))
    
    # initialize vectors for stats
    n_cols   <- length(var_names)
    mean_var <- rep(NA, n_cols)
    sd_var   <- rep(NA, n_cols)
    min_var  <- rep(NA, n_cols)
    max_var  <- rep(NA, n_cols)
    n_levels <- rep(NA, n_cols)
    mode_var <- rep(NA, n_cols)
    pct_mode <- rep(NA, n_cols)
    
    # loop over each column to compute numeric or factor stats
    for (i in seq_along(var_names)) {
      col <- df[[i]]

      if (is.numeric(col) || is.integer(col)) {
        ## numeric summary
        mean_var[i] <- round(mean(col, na.rm = TRUE), 2)
        sd_var[i]   <- round(sd(col, na.rm = TRUE), 2)
        min_var[i]  <- round(min(col, na.rm = TRUE), 2)
        max_var[i]  <- round(max(col, na.rm = TRUE), 2)

      } else if (is.factor(col) || is.character(col)) {
        ## categorical summary
        fac_col       <- as.factor(col)
        n_levels[i]   <- nlevels(fac_col)
        tab_freq      <- sort(table(fac_col), decreasing = TRUE)
        mode_var[i]   <- names(tab_freq)[1]
        pct_mode[i]   <- round(as.numeric(tab_freq[1]) / length(col) * 100, 2)
        }
    }
    
    # assemble into a data.frame
    var_summary <- data.frame(
      Variable       = var_names,
      Class          = var_classes,
      `Unique values` = n_unique,
      `Missing (%)`  = pct_miss,
      Mean           = mean_var,
      `Std Dev`      = sd_var,
      Min            = min_var,
      Max            = max_var,
      `Num. Levels`  = n_levels,
      Mode           = mode_var,
      `Mode (%)`     = pct_mode,
      stringsAsFactors = FALSE,
      check.names     = FALSE
    )
    
    # render as an interactive DT
    datatable(
      var_summary,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  

  ## 2.3 data preview ----
  output$tablePreview <- renderDT({
    req(dataset(), selectedVariables())
    df <- dataset()[, intersect(selectedVariables(), names(dataset())), drop = FALSE]
    datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })

  output$var2_ui <- renderUI({
    req(input$analysis_method)
    # just for bivariate methods
    if (input$analysis_method %in% c("Contingency Table", "Bivariate Scatter")) {
      selectInput("analysis_var2", "Variable 2:", choices = selectedVariables())
    }
  })
  
  ## 2.4 edit data ----
  
  # reactiveVal to hold the editable data.frame (always the selected subset)
  editable_df <- reactiveVal(NULL)
  
  # history stack for “Undo Last Action”
  edit_history <- reactiveValues(stack = list())

  push_history <- function() {
    current_stack <- isolate(edit_history$stack)
    edit_history$stack <- append(current_stack, list(isolate(editable_df())))
  }
  
  # reset the editable_df whenever the base data or selection changes
  observeEvent(c(dataset(), selectedVariables()), {
    req(dataset())
    full_df <- dataset()
    sel_vars <- selectedVariables()

    if (length(sel_vars) == 0) {
      editable_df(data.frame()) # empty if nothing selected
      } else {
      df_subset <- full_df[, sel_vars, drop = FALSE]
      editable_df(df_subset)
      }
    
    edit_history$stack <- list() # clear undo history on reset
    })
  
  # render the rhandsontable for editing
  output$editable_table <- renderRHandsontable({
    req(editable_df())
    rhandsontable(
      editable_df(),
      height   = 600,
      width    = "100%",
      stretchH    = "all") %>% 
      hot_table(contextMenu = TRUE, readOnly = FALSE)
    })
  
  # sync manual edits back into reactiveVal
  observeEvent(input$editable_table, {
    df_mod <- hot_to_r(input$editable_table)
    editable_df(df_mod)
  })
  
  ### a. undo last action ----
  observeEvent(input$undo_edit, {
    curr_stack <- edit_history$stack
    if (length(curr_stack) == 0) {
      showModal(modalDialog(
        title = "Nothing to Undo",
        "You have no previous edits to undo.",
        easyClose = TRUE
        ))
      return()
      }
    last_version <- curr_stack[[length(curr_stack)]]
    new_stack <- curr_stack[-length(curr_stack)]
    edit_history$stack <- new_stack
    editable_df(last_version)
    })
  
  ### b. rename column ----
  observeEvent(input$rename_column, {
    req(editable_df())
    cols <- names(editable_df())
    showModal(modalDialog(
      title = "Rename Column",
      selectInput("rename_col_select", "Choose column to rename:", choices = cols),
      textInput("rename_col_new",    "New column name:", value = ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_rename_col", "Rename")
        ),
      easyClose = TRUE
      ))
    })
  
  
  observeEvent(input$confirm_rename_col, {
    req(input$rename_col_select, input$rename_col_new)
    removeModal()
    push_history()
    
    df <- editable_df()
    old_name <- input$rename_col_select
    new_name <- input$rename_col_new
    
    # name validation
    if (!nzchar(new_name)) {
      showModal(modalDialog(
        title = "Invalid Name",
        "The new column name cannot be empty.",
        easyClose = TRUE
      ))
      return()
    }
    if (new_name %in% names(df)) {
      showModal(modalDialog(
        title = "Name Already Exists",
        "That column name is already in use. Choose a different one.",
        easyClose = TRUE
      ))
      return()
    }
    
    # apply rename
    colnames(df)[ colnames(df) == old_name ] <- new_name
    editable_df(df)
    
    # keep the selection vector in sync
    sel <- selectedVariables()
    if (old_name %in% sel) {
      sel[ sel == old_name ] <- new_name
      selectedVariables(sel)
      }
    
    })
  
  
  ### c. drop rows with NA values ----
  observeEvent(input$drop_na_rows, {
    req(editable_df())
    push_history()
    showModal(modalDialog(
      title = "Confirm Drop Rows with NA",
      "This will remove any row that contains at least one missing (NA) value. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_drop_na", "Yes, continue")
        ),
      easyClose = FALSE
      ))
    })
  
  observeEvent(input$confirm_drop_na, {
    removeModal()
    df2 <- editable_df() %>%
      tidyr::drop_na()
    editable_df(df2)
    })
  
  ### d. character columns to factors ----
  observeEvent(input$convert_factors, {
    req(editable_df())
    push_history()
    showModal(modalDialog(
      title = "Confirm Convert Characters to Factors",
      "This will convert ALL character columns to factor type. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_convert_factors", "Yes, continue")
        ),
      easyClose = FALSE
      ))
    })
  
  observeEvent(input$confirm_convert_factors, {
    removeModal()
    df2 <- editable_df() %>% 
      dplyr::mutate(across(where(is.character), as.factor))
    editable_df(df2)
    })

  ### e. fill NAs in character col. with "Unknown" ----
  observeEvent(input$fill_na_unknown, {
    req(editable_df())
    push_history()
    showModal(modalDialog(
      title = "Confirm Fill NAs with 'Unknown'",
      "This will replace all NA values in character columns with 'Unknown'. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_fill_na_unknown", "Yes, continue")
        ),
      easyClose = FALSE
      ))
    })
  
  observeEvent(input$confirm_fill_na_unknown, {
    removeModal()
    df <- editable_df()
    df2 <- df %>% 
      dplyr::mutate(across(where(is.character), ~ ifelse(is.na(.x), "Unknown", .x)))
    editable_df(df2)
    })
  
  ### f. fill NAs in numeric col. with mean ----
  observeEvent(input$fill_na_mean, {
    req(editable_df())
    push_history()
    showModal(modalDialog(
      title = "Confirm Fill NAs with Mean",
      "This will replace all NA values in each numeric column with that column’s mean. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_fill_na_mean", "Yes, continue")
        ),
      easyClose = FALSE
      ))
    })
  
  observeEvent(input$confirm_fill_na_mean, {
    removeModal()
    df <- editable_df()
    df2 <- df %>% 
      dplyr::mutate(
        across(
          where(is.numeric),
          ~ ifelse(is.na(.x), round(mean(.x, na.rm = TRUE), 2), .x)
          )
        )
    editable_df(df2)
    })
  
  
  ### g. parse dates ----
  observeEvent(input$parse_dates, {
    req(editable_df())
    push_history()
    showModal(modalDialog(
      title = "Confirm Parse Dates",
      "This will attempt to parse any column that looks like YYYY-MM-DD into a Date object. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_parse_dates", "Yes, continue")
        ),
      easyClose = FALSE
      ))
    })
  
  observeEvent(input$confirm_parse_dates, {
    removeModal()
    df <- editable_df()
    
    # first normalize strings
    df2 <- df %>%
      dplyr::mutate(
        across(
          where(is.character),
          ~ {
            parsed <- lubridate::ymd(.x, quiet = TRUE)
            ifelse(is.na(parsed), .x, format(parsed, "%Y-%m-%d"))
            }
          )
        )
    
    # then convert pure YYYY-MM-DD columns to Date
    df3 <- df2 %>%
      dplyr::mutate(
        across(
          where(~ all(grepl("^\\d{4}-\\d{2}-\\d{2}$", .x, perl = TRUE), na.rm = TRUE)),
          ~ as.Date(.x)
          )
        )
    editable_df(df3)
    })
  
  ### h. recode single values ----
  observeEvent(input$recode_values, {
    req(editable_df())

    showModal(
      modalDialog(
        title = "Recode Values",
        
        # select column to recode
        selectInput(
          inputId = "recode_col_select",
          label   = "Column to recode:",
          choices = names(editable_df())
          ),
        
        # select old value
        selectInput(
          inputId = "recode_old_value",
          label   = "Old value:",
          choices = NULL
          ),
        
        # write new value
        textInput(
          inputId = "recode_new_value",
          label   = "New value:",
          value   = ""
          ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_recode", "Apply Recode")
          ),
        
        easyClose = TRUE,
        size      = "m"
        )
      )
    
    # update the “old value” dropdown as soon as the user picks a column
    observeEvent(input$recode_col_select, {
      col_selec <- input$recode_col_select
      posibles_vals <- unique(as.character(editable_df()[[col_selec]]))
      updateSelectInput(
        session,
        "recode_old_value",
        choices = posibles_vals,
        selected = posibles_vals[1]
        )
      }, ignoreInit = TRUE)
    })
  
  # confirm recodification
  observeEvent(input$confirm_recode, {
    req(input$recode_col_select, input$recode_old_value, input$recode_new_value)
    removeModal()
    push_history()
    
    df <- editable_df()
    col_name <- input$recode_col_select
    old_val  <- input$recode_old_value
    new_val  <- input$recode_new_value
    
    # try to recode safely
    tryCatch({
      df2 <- df %>%
        dplyr::mutate(
          !!rlang::sym(col_name) := dplyr::recode(
            !!rlang::sym(col_name),
            !!old_val := new_val
            )
          )
      editable_df(df2)
      }, 
      
      error = function(e) {
      showModal(modalDialog(
        title = "Recode Error",
        paste0("Failed to recode:\n", e$message),
        easyClose = TRUE
        ))
      })
    })
  
  ### i.  save changes ----
  observeEvent(input$save_edits, {
    req(editable_df())
    dataset(editable_df())
    
    selectedVariables(intersect(selectedVariables(), names(editable_df())))
    
    edit_history$stack <- list()
    showModal(modalDialog(
      title = "Changes Saved",
      "The data has been updated with your edits.",
      easyClose = TRUE
    ))
  })
  
  
  # 3. Data analysis ----
  
  ## 3.1 update seleted variables ----
  
  # whenever selectedVariables() changes, reset analysis_var1 choices
  observeEvent(selectedVariables(), {
    vars <- selectedVariables()
    updateSelectInput(session, "analysis_var1",
                      choices  = vars,
                      selected = if (length(vars)) vars[1] else NULL)
    })
  
  # filter inputs when method or selectedVariables() changes
  observeEvent(
    list(input$analysis_method, selectedVariables()),
    {
      req(dataset())
      df     <- dataset()
      method <- input$analysis_method
      sel    <- selectedVariables()
      
      # — Variable 1 —
      valid1 <- intersect(get_valid_vars(df, method), sel)
      updateSelectInput(session,
                        "analysis_var1",
                        choices  = valid1,
                        selected = if (length(valid1)) valid1[1] else NULL
      )
      
      # — Variable 2 (just for bivariate andcontingency table) —
      if (method %in% c("Contingency Table", "Bivariate Scatter")) {
        valid2 <- intersect(get_valid_vars(df, method), sel)
        output$var2_ui <- renderUI({
          selectInput("analysis_var2", "Variable 2:", choices = valid2)
          })
        } else {
        
        # hide the second‐variable UI for univariate methods
        output$var2_ui <- renderUI(NULL)
        }
      }
    )
  
  
  ## 3.2  run & render analysis ----
  
  # disable “Save Analysis” until we have a fresh result
  shinyjs::disable("save_analysis")
  
  # container for last analysis result
  vals <- reactiveValues(last_res = NULL)
  
  # run analysis when click on button
  observeEvent(input$run_analysis, {
    req(input$analysis_var1)
    vals$last_res <- perform_analysis(
      dataset(),
      input$analysis_method,
      input$analysis_var1,               # first variable
      input$analysis_var2                # second variable, or NULL
      )
    })
  
  # enable “Save Analysis” as soon as we have a results
  observeEvent(vals$last_res, {
    if (!is.null(vals$last_res)) shinyjs::enable("save_analysis")
  })
  
  # tell UI if we have a plot
  output$has_plot <- reactive({
    !is.null(vals$last_res) && !is.null(vals$last_res$plot)
    })
  outputOptions(output, "has_plot", suspendWhenHidden = FALSE)
  
  ## render table if there is no plot
  output$analysis_table <- renderDT({
    req(vals$last_res)
    req(is.null(vals$last_res$plot))
    datatable(vals$last_res$result, options = list(pageLength = 5, scrollX = TRUE))
    })
  
  ## render plot if exists
  output$analysis_plot <- renderPlot({
    req(vals$last_res)
    req(!is.null(vals$last_res$plot))
    print(vals$last_res$plot)
    })
  

  ## 3.3 save analysis ----
  
  # save current analysis
  observeEvent(input$save_analysis, {
    req(vals$last_res)
    new_res <- vals$last_res
    new_res$id <- paste0("anal_", as.integer(Sys.time()))
    # descriptive label
    new_res$label <- if (!is.null(new_res$variable2)) {
      paste0(new_res$method, " – ", new_res$variable, " vs ", new_res$variable2)
      } else {
      paste0(new_res$method, " – ", new_res$variable)
        }
    analysis_storage( append(analysis_storage(), list(new_res)) )
    shinyjs::disable("save_analysis")
    })
  
  
  ## 3.4  list saved analyses ----
  output$analysis_list <- renderUI({
    lst <- analysis_storage()
    validate(
      need(is.list(lst),  ""),
      need(length(lst) >= 0, "")
    )
    if (length(lst) == 0) {
      return(tags$p("No saved analyses."))
    }

    tagList(
      lapply(lst, function(a) {
        div(style="margin-bottom:6px; display:flex; gap:4px; align-items:center;",
            tags$span(a$label, style="font-weight:bold; font-size:90%;"),
            actionButton(
              inputId = paste0("view_", a$id),
              label   = NULL,
              icon    = icon("eye", class = "fa-sm"),
              title   = "Ver",
              class   = "btn btn-default btn-xs"
            ),
            actionButton(
              inputId = paste0("delete_", a$id),
              label   = NULL,
              icon    = icon("trash-alt", class = "fa-sm"),
              title   = "Borrar",
              class   = "btn btn-default btn-xs",
              style   = "color:#d9534f;"
            ),
            actionButton(
              inputId = paste0("rename_", a$id),
              label   = NULL,
              icon    = icon("edit", class = "fa-sm"),
              title   = "Renombrar",
              class   = "btn btn-default btn-xs",
              style   = "color:#f0ad4e;"
            )
            )
        })
      )
    })
  
  ## saved‐analysis buttons
  observe({
    lst <- analysis_storage()
    
    lapply(lst, function(a) {
      local({
        this_id <- a$id
        
        ### View ----
        observeEvent(input[[paste0("view_", this_id)]], {
          full_lst <- analysis_storage()
          sel_idx  <- which(vapply(full_lst, function(x) x$id, FUN.VALUE = "") == this_id)
          if (length(sel_idx)==1) {
            sel <- full_lst[[ sel_idx ]]
            showModal(modalDialog(
              title = sel$label,
              if (!is.null(sel$plot)) {
                plotOutput(paste0("modal_plot_", this_id), height = "400px")
              } else {
                output[[paste0("modal_dt_", this_id)]] <- renderDT({
                  datatable(
                    sel$result,
                    options = list(pageLength = nrow(sel$result),
                                   scrollY = '300px', 
                                   scrollX = TRUE,
                                   dom = 't'),
                    rownames = FALSE
                  )
                })
              },
              footer = modalButton("Close"), size = "l"
            ))
            
            if (!is.null(sel$plot)) {
              output[[paste0("modal_plot_", this_id)]] <- renderPlot({
                print(sel$plot)
              })
            } else {
              output[[paste0("modal_dt_", this_id)]] <- renderDT({
                datatable(sel$result, options = list(dom = 't', pageLength = 5), rownames = FALSE)
              })
            }
          }
        }, ignoreInit = TRUE)
        
        ### Delete ----
        observeEvent(input[[paste0("delete_", this_id)]], {
          new_lst <- Filter(function(x) x$id != this_id, analysis_storage())
          analysis_storage(new_lst)
        }, ignoreInit = TRUE)
        
        ### Rename ----
        observeEvent(input[[paste0("rename_", this_id)]], {
          values$rename_id <- this_id
          showModal(modalDialog(
            title = "Rename analysis",
            textInput("new_label", "New label:", value = a$label),
            footer = tagList(modalButton("Cancel"),
                             actionButton("confirm_rename", "Confirm"))
          ))
        }, ignoreInit = TRUE)
      })
    })
  })
  
  
  # apply rename once user confirms
  observeEvent(input$confirm_rename, {
    i <- values$rename_index
    nuevo <- isolate(input$new_label)
    lst <- analysis_storage()
    if (!is.null(i) && i <= length(lst) && nzchar(nuevo)) {
      lst[[i]]$label <- nuevo
      analysis_storage(lst)
      }
    removeModal()
    })
  
  
  ## 3.5 recalculate analysis ----
  observeEvent(dataset(), {
    req(dataset())
    # 1) Recompute every saved analysis
    updated <- lapply(analysis_storage(), function(anal) {
      nr <- perform_analysis(dataset(), anal$method, anal$variable, anal$variable2)
      anal$result <- nr$result
      anal$plot   <- nr$plot
      anal
    })
    analysis_storage(updated)
  })
  
  
  # 4. Dashboard ----
  
  ## 4.1 summary table----
  output$data_summary <- renderDT({
    req(dataset(), selectedVariables())
    df_all       <- dataset()
    sel_vars     <- selectedVariables()
    
    # if no variables selected, empty table
    if (length(sel_vars) == 0) {
      return(datatable(
        data.frame(Message = "No variables selected"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # subset to only the selected columns
    df <- df_all[, sel_vars, drop = FALSE]
    
    # compute metrics
    var_names   <- names(df)
    var_classes <- sapply(df, function(col) class(col)[1])
    n_unique    <- sapply(df, function(col) length(unique(col)))
    pct_miss    <- sapply(df, function(col) round(sum(is.na(col)) / length(col) * 100, 2))
    
    # initialize vectors for stats
    n_cols   <- length(var_names)
    mean_var <- rep(NA, n_cols)
    sd_var   <- rep(NA, n_cols)
    min_var  <- rep(NA, n_cols)
    max_var  <- rep(NA, n_cols)
    n_levels <- rep(NA, n_cols)
    mode_var <- rep(NA, n_cols)
    pct_mode <- rep(NA, n_cols)
    
    # loop over each column to compute numeric or factor stats
    for (i in seq_along(var_names)) {
      col <- df[[i]]
      
      if (is.numeric(col) || is.integer(col)) {
        ## numeric summary
        mean_var[i] <- round(mean(col, na.rm = TRUE), 2)
        sd_var[i]   <- round(sd(col, na.rm = TRUE), 2)
        min_var[i]  <- round(min(col, na.rm = TRUE), 2)
        max_var[i]  <- round(max(col, na.rm = TRUE), 2)
        
      } else if (is.factor(col) || is.character(col)) {
        ## categorical summary
        fac_col       <- as.factor(col)
        n_levels[i]   <- nlevels(fac_col)
        tab_freq      <- sort(table(fac_col), decreasing = TRUE)
        mode_var[i]   <- names(tab_freq)[1]
        pct_mode[i]   <- round(as.numeric(tab_freq[1]) / length(col) * 100, 2)
      }
    }
    
    # assemble into a data.frame
    var_summary <- data.frame(
      Variable       = var_names,
      Class          = var_classes,
      `Unique values` = n_unique,
      `Missing (%)`  = pct_miss,
      Mean           = mean_var,
      `Std Dev`      = sd_var,
      Min            = min_var,
      Max            = max_var,
      `Num. Levels`  = n_levels,
      Mode           = mode_var,
      `Mode (%)`     = pct_mode,
      stringsAsFactors = FALSE,
      check.names     = FALSE
    )
    
    # render as an interactive DT
    datatable(
      var_summary,
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  ## 4.2 project info ----
  
  # - title -
  output$proj_title <- renderText({
    if (!is.null(values$project_data)) {
      return(values$project_data$name)
      }
    if (is.null(input$project_name) || input$project_name == "") {
      return("(none)")
      }
    input$project_name
    })
    
  # - client -
  output$proj_client <- renderText({
    if (!is.null(values$project_data)) {
      return(values$project_data$client)
      }
    if (is.null(input$client_name) || input$client_name == "") {
      return("(none)")
      }
    input$client_name
    })
  
  # - date - 
  output$proj_date <- renderText({
    if (!is.null(values$project_data)) {
      return(as.character(values$project_data$date))
      }
    format(Sys.Date(), "%Y-%m-%d")
    })
  
  
  ##  4.3 placeholder configuration ----
  openModalForPlaceholder <- function(box_id) {
    
    # ensure there is at least one saved analysis
    if (length(analysis_storage()) == 0) {
      showModal(
        modalDialog(
          title = "No analyses available",
          "There are no saved analyses to display. Please save at least one analysis first.",
          easyClose = TRUE
          )
        )
      return()
      }
    
    # build named list
    all_analyses <- analysis_storage()
    choices <- setNames(
      seq_along(all_analyses),
      sapply(all_analyses, function(a) {
        if (!is.null(a$label)) a$label else paste0(a$method, " – ", a$variable)
        })
      )
    
    showModal(
      modalDialog(
        title = paste0("Configure Box ", box_id),
        
        # 1) which saved analysis
        selectInput(
          inputId = paste0("select_analysis_", box_id),
          label   = "Select saved analysis:",
          choices = choices
          ),
        
        # 2) select display
        # just if its not a method with graph
        conditionalPanel(
          condition = sprintf(
            "!(/^Histogram|Boxplot|Bivariate Scatter/.test($('#select_analysis_%s option:selected').text()))",
            box_id
          ),
          radioButtons(
            inputId = paste0("view_mode_", box_id),
            label   = "View as:",
            choices = c("Table","Plot"),
            selected = "Table"
          )
        ),
        
        conditionalPanel(
          condition = sprintf(
            "input.view_mode_%1$s=='Plot' && !(/^Histogram|Boxplot|Bivariate Scatter/.test($('#select_analysis_%1$s option:selected').text()))",
            box_id
          ),
          selectInput(
            inputId = paste0("chart_type_", box_id),
            label   = "Chart type:",
            choices = c("Bar","Pie","Line"),
            selected = "Bar"
          )
        ),
        
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = paste0("confirm_box_", box_id),
            label   = "Insert"
            )
          ),
        easyClose = TRUE,
        size      = "m"
        )
      )
    }
  
  
  # wire each placeholder button to open the modal
  observeEvent(input$add_1, { openModalForPlaceholder("1") })
  observeEvent(input$add_2, { openModalForPlaceholder("2") })
  observeEvent(input$add_3, { openModalForPlaceholder("3") })
  observeEvent(input$add_4, { openModalForPlaceholder("4") })
  
  
  # ## 4.4 insert analysis ----
  lapply(1:4, function(i) {
    observeEvent(input[[paste0("confirm_box_", i)]], {
      removeModal()
      
      sel_idx <- as.numeric(input[[paste0("select_analysis_", i)]])
      vm_id   <- paste0("view_mode_",  i)
      ct_id   <- paste0("chart_type_", i)
      
      # look at analysis method
      method_i <- analysis_storage()[[sel_idx]]$method
      
      # if its a graph method, we forced it
      if (method_i %in% c("Histogram", "Boxplot", "Bivariate Scatter")) {
        mode   <- "Plot"
        c_type <- NULL
      } else {
        # rest cases
        mode   <- if (!is.null(input[[vm_id]])) input[[vm_id]] else "Table"
        c_type <- if (mode == "Plot" && !is.null(input[[ct_id]])) input[[ct_id]] else NULL
      }
      
      # build item and put it on dashboard_items()
      box_id <- paste0("box_", i, "_", sample(1e5:1e6,1))
      itm <- list(id = box_id, slot = i, source_idx = sel_idx, view = mode, chart_type = c_type)
      dashboard_items( append(dashboard_items(), list(itm)) )
      
      # render
      insertUI(
        selector = paste0("#ph_", i),
        where    = "afterEnd",
        ui       = dashboard_box(itm, analysis_storage()[[sel_idx]])
      )
      
      if (mode == "Table") {
        output[[paste0("out_", box_id)]] <- renderDT({
          dat <- analysis_storage()[[sel_idx]]$result
          datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        })
      } else {
        output[[paste0("out_", box_id)]] <- renderPlot({
          fresh <- analysis_storage()[[sel_idx]]
          # if ggplot, print it
          if (!is.null(fresh$plot)) {
            print(fresh$plot)
            return()
          }
          vals <- fresh$result
          labs <- if (fresh$method == "Frequency") fresh$result$Value else names(vals)
          if (c_type == "Bar") {
            barplot(vals, names.arg = labs, las = 2, main = fresh$label)
          } else if (c_type == "Pie") {
            pie(vals, labels = labs, main = fresh$label)
          } else {
            plot(vals, type = "o", xaxt = "n", ylab = fresh$method, main = fresh$label)
            axis(1, at = seq_along(vals), labels = labs, las = 2)
          }
        })
      }
      
      shinyjs::hide(selector = paste0("#ph_", i))
    }, ignoreInit = TRUE)
  })
  
  
  ## 4.5 rebuild dashboard_items ----
  observeEvent(dashboard_items(), {
    if (input$sidebar_tabs == "dashboard_view") {
      rebuild_dashboard_ui(
        items               = dashboard_items(),
        analysis_list       = analysis_storage(),
        dashboard_items_rv  = dashboard_items,
        input               = input,
        output              = output,
        session             = session
        )
      }
    }, ignoreNULL = FALSE)

  
  # 5. Export ----
  
  output$download_ppt_selected <- downloadHandler(
    
    ## 5.1 pre-configuration ----
    
    # file name
    filename = function() {
      paste0("Dashboard_", Sys.Date(), ".pptx")
    },
    
    # MIME type for PowerPoint
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    
    # content: function that actually generates the file at the given path
    content = function(file) {
      # grab dashboard_items
      items <- dashboard_items()
      # if there are none, abort
      if (length(items) == 0) {
        showModal(modalDialog(
          title = "Nothing to Export",
          "You have not added any containers to the dashboard yet.",
          easyClose = TRUE
          ))
        return(NULL)
        }
      
      ## 5.2 new pptx ----
      ppt <- read_pptx()
      
      # loop over each dashboard_item
      for (item in items) {
        anal <- analysis_storage()[[ item$source_idx ]]
        etiqueta <- if (!is.null(anal$label)) anal$label
        else paste0(anal$method, " – ", anal$variable)
        
        # - table- 
        if (item$view == "Table") {
          if (anal$method == "Frequency") {
            df_tabla <- anal$result
          } else {
            df_tabla <- data.frame(
              Estadística = anal$method,
              Valor        = anal$result,
              stringsAsFactors = FALSE
            )
          }
          
          ## create and autofit the flextable
          ft_tabla <- flextable::flextable(df_tabla) %>% flextable::autofit()
          
          ## add a new slide, put title and table
          ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
          ppt <- officer::ph_with(ppt, etiqueta, officer::ph_location_type(type = "title"))
          ppt <- officer::ph_with(ppt, ft_tabla,  officer::ph_location_type(type = "body"))
          
        } else {
          
        
          # - Plot -
          ## render the plot to a temporary PNG
          tmp_png <- tempfile(fileext = ".png")
          png(tmp_png, width = 800, height = 600)
          
          ## draw chart type
          if (!is.null(anal$plot)) {
            print(anal$plot)
          } else {
            
            if (item$chart_type == "Bar") {
              if (anal$method == "Frequency") {
              barplot(
                anal$result$Frequency,
                names.arg = anal$result$Value,
                main      = etiqueta,
                las       = 2
                )
              } else {
                barplot(
                  anal$result,
                  names.arg = anal$method,
                  main      = etiqueta,
                  )
                }
            } else if (item$chart_type == "Pie") {
              if (anal$method == "Frequency") {
                pie(
                  anal$result$Frequency,
                  labels = anal$result$Value,
                  main   = etiqueta,
                  col    = rainbow(nrow(anal$result))
                  )
                } else {
                  pie(
                    anal$result,
                    labels = anal$method,
                    main   = etiqueta,
                    col    = rainbow(1)
                    )
                  }
              } else if (item$chart_type == "Line") {
                if (anal$method == "Frequency") {
                  plot(
                    anal$result$Frequency,
                    type = "o",
                    xaxt = "n",
                    ylab = "Frequency",
                    main = etiqueta,
                    )
                  axis(1, at = seq_len(nrow(anal$result)), labels = anal$result$Value, las = 2)
                  } else {
                    plot(
                      anal$result,
                      type = "o",
                      main = etiqueta,
                      ylab = anal$method,
                      )
                    axis(1, at = 1, labels = anal$method)
                  }
              }
            }
          
          ## finish writing the PNG
          dev.off()
          
          ## add slide, insert image
          ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
          ppt <- officer::ph_with(ppt, etiqueta, officer::ph_location_type(type = "title"))
          ppt <- officer::ph_with(
            ppt,
            officer::external_img(src = tmp_png, width = 6, height = 4),
            officer::ph_location_type(type = "body")
            )
          
          ## clean up temporal file
          unlink(tmp_png)
          }
        }
      
      # write the pptx to disk 
      print(ppt, target = file)
      }
    )

  
  
  # 0. Others----
  
  ## 1. Permanent home view ----
  # show main buttons (new project, previous) whenever clicking on home
  observeEvent(input$sidebar_tabs, {
    if (input$sidebar_tabs == "home") {
      hide("formContainer")
      show("main_buttons")
    }
  })
  
  ## 2. Save full project ----
  observeEvent(input$save_full_progress, {
    
    # validate dataset and some variables are selected
    if (is.null(dataset()) || is.null(selectedVariables())) {
      showModal(modalDialog(
        title = "Error",
        "There is no project loaded or created to save",
        easyClose = TRUE
        ))
      return()  # abort if nothing to save
      }
    
    # call save_project to re-write what needed
    save_project(
      dataset           = dataset,
      selectedVariables = selectedVariables,
      input             = input,
      values            = values,
      analysis_list    = analysis_storage(),
      dashboard_items_reactive = dashboard_items
      )
    
    #  build the path to the newly written .rds file
    project_file <- file.path(
      "saved_projects",
      paste0(input$project_name, ".rds")
      )
    
    # re-load that same RDS
    if (file.exists(project_file)) {
      values$project_data <- readRDS(project_file)
    }
    
    # load project
    load_project(
      project                  = values$project_data,
      session                  = session,
      dataset                  = dataset,
      selectedVariables        = selectedVariables,
      analysis_storage         = analysis_storage,
      dashboard_items_reactive = dashboard_items,
      input                    = input,
      output                   = output
    )
    })
  
  
  ## 3. Clean dashboard if saved analysis changes -----
  # cleanup observer that runs whenever you add/delete/rename an analysis
  observeEvent(analysis_storage(), {
    valid_idx <- seq_along(analysis_storage())
    items     <- dashboard_items()
    
    # filter out‐of‐bounds
    items <- Filter(\(it) it$source_idx %in% valid_idx, items)
    
    # readjust index
    items <- lapply(items, \(it) {
      it$source_idx <- match(it$source_idx, valid_idx)
      it
    })
    
    # save and rebuild
    dashboard_items(items)
    if (input$sidebar_tabs == "dashboard_view") {
      rebuild_dashboard_ui(
        items               = items,
        analysis_list       = analysis_storage(),
        dashboard_items_rv  = dashboard_items,
        input               = input,
        output              = output,
        session             = session
      )
    }
  }, ignoreNULL = FALSE)
  
  
  ## 4. Rebuild dashbord UI ----
  # re-build the dashboard UI any time items change or the user returns to the tab
  observeEvent(
    list(dashboard_items(), input$sidebar_tabs),
    {
      if (input$sidebar_tabs == "dashboard_view") {
        rebuild_dashboard_ui(
          items               = dashboard_items(),
          analysis_list       = analysis_storage(),
          dashboard_items_rv  = dashboard_items,
          input               = input,
          output              = output,
          session             = session
          )
        }
      },
    ignoreNULL = TRUE
  )
  
  
  
}


