## Andrea Rivera Mateos
## Date: June 2025
## Project: From Data to Report

# Libraries ----


rm(list = ls()) # Remove old variables

packages = c("shiny", "shinyjs", "DT", "readxl", "dplyr", "haven", "officer", "flextable", 
             "shinydashboard", "rhandsontable", "ggplot2", "tidyr", "lubridate", "rlang", "dplyr")

package.check <- lapply(packages,
                        FUN = function(x){
                          if (!require(x,character.only = TRUE)){
                            install.packages(x,dependencies = TRUE)
                            library(x, character.only = TRUE)
                            }
                          }
                        )


# Functions ----

## Read data ----

read_datafile <- function(datafile) {
  
  # csv files
  if (grepl(".csv$", datafile$name)) {
    data <- tryCatch({
      read.csv(datafile$datapath, fill = TRUE)
      }, error = function(e) return(NULL))
    
    # xlsx files
    } else if (grepl(".xlsx$", datafile$name)) {
    data <- tryCatch({
      read_excel(datafile$datapath)
      }, error = function(e) return(NULL))
    
    # xls files
    } else if (grepl(".xls$", datafile$name)) {
    data <- tryCatch({
      read_excel(datafile$datapath)
    }, error = function(e) return(NULL))
    
    # dta (STATA) files
    } else if (grepl(".dta$", datafile$name)) {
    data <- tryCatch({
      read_dta(datafile$datapath)
    }, error = function(e) return(NULL))
    
    # unsupported format file
    } else {
      showModal(modalDialog(
        title = "Error",
        "Unsupported file format. Please upload a csv, xls, xlsx, or dta.",
        easyClose = TRUE,
        footer = NULL
        ))
      return(NULL)
    }
  
  return(data)
  
  }


## Clean data ----

clean_dataset <- function(data) {
  data %>%
    dplyr::select(where(~ is.numeric(.) || is.character(.) || is.factor(.))) %>%
    dplyr::mutate(across(where(is.character), as.factor))
  }


## Save project ----

save_project <- function(dataset, selectedVariables, input, values, analysis_list, 
                         dashboard_items_reactive) {
  
  # 1. Make sure we have data and varaibles selected
  req(dataset, selectedVariables)
  
  # 2. Take project name from UI
  project_name <- input$project_name
  
  # 3. Create save folder (if needed)
  if (!dir.exists("saved_projects")) dir.create("saved_projects")
  
  # 4. Bundle up everything we need to restore
  saveRDS(
    list(
      name            = isolate(input$project_name),        # project tittle
      client          = isolate(input$client_name),         # client's name
      data            = dataset(),                          # active dataset
      variables       = selectedVariables(),                # selected variables
      analyses        = analysis_list,                      # saved analysis list
      dashboard_items = dashboard_items_reactive(),         # dashboard configuration
      date            = Sys.Date(),                         # current date
      file            = values$project_file_name            # original file name
      ),
    
    # 5. Save that state as an .rds file
    file = file.path("saved_projects", paste0(project_name, ".rds"))
    )
  
  # 6. Confirmation for user 
  showModal(modalDialog(
    title = "Save Complete",
    "The project and its analyses have been saved successfully.",
    easyClose = TRUE
  ))
  
  }


## Load project ----


load_project <- function(project, session, dataset, selectedVariables, analysis_storage, 
                         dashboard_items_reactive, input, output) {
  
  # 1. Restore dataset and user's selected variables
  dataset(project$data)
  selectedVariables(project$variables)
  
  # 2. Restore saved analysis
  lst <- project$analyses
  
  ## – if there are none, initialize an empty list
  if (is.null(lst) || length(lst) == 0) {
    lst <- list()
    } else {
      ##    – ensure every analysis has a label
      lst <- lapply(lst, function(anal) {
      if (is.null(anal$label)) {
        anal$label <- paste0(anal$method, " – ", anal$variable)
        }
        anal
      })
      }
  analysis_storage(lst)
  
  # 3. Restore dashboard configuration
  db <- project$dashboard_items
  ## – Default to an empty list if nothing was saved
  if (is.null(db) || length(db) == 0) {
    db <- list()
    }
  dashboard_items_reactive(db)
  
  # 4. Rebuild all of the dashboard UI
  rebuild_dashboard_ui(
    items               = db,
    analysis_list       = analysis_storage(),
    dashboard_items_rv  = dashboard_items_reactive,
    input               = input,
    output              = output,
    session             = session
    )
  
  # 5. Update project name ad client name show the loaded values
  updateTextInput(session, "project_name", value = project$name)
  updateTextInput(session, "client_name",  value = project$client)
  
  }


## Perform analysis ----

perform_analysis <- function(dataset, analysis_method, var1, var2 = NULL) {
  
  # 1. Ensure we have a dataset and at least one variable
  req(dataset, var1)
  
  # 2. Local copies for ease of use
  method <- analysis_method
  df1    <- dataset[[var1]] # first variable’s vector
  df2    <- if(!is.null(var2)) dataset[[var2]] else NULL # second variable (if any)
  
  # 3. Initialize outputs
  df_result <- NULL # for tabular results
  plt       <- NULL  # for ggplot objects
  
  # 4. Analysis methods
  switch(method,

         ## 4.1 Univariate summary tables
         "Frequency" = {
           req(!is.null(var1))
           vec <- dataset[[var1]]
           
           tab <- table(vec)
           df_result <- data.frame(Value = names(tab), Frequency = as.vector(tab))
           
           plt <- ggplot(df_result, aes(x = reorder(Value, -Frequency), y = Frequency)) +
             geom_bar(stat = "identity", fill = "steelblue", alpha = 0.85) +
             labs(
               title    = paste("Bar Plot of", var1),
               subtitle = "Frequencies of observed values",
               x        = var1,
               y        = "Frequency"
             ) +
             theme_minimal(base_size = 14) +
             theme(
               plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
               plot.subtitle    = element_text(size = 12, hjust = 0.5, color = "grey40"),
               axis.title       = element_text(face = "bold"),
               axis.text.x      = element_text(angle = 45, hjust = 1),
               panel.grid.major = element_line(color = "grey90"),
               panel.grid.minor = element_blank()
             )
           
           list(
             method   = "Frequency",
             variable = var1,
             result   = df_result,
             plot     = plt
           )
         },
         
         "Mean" = {
           req(is.numeric(df1))
           df_result <- data.frame(Statistic = "Mean", Value = round(mean(df1, na.rm = TRUE), 2))
           },
         
         "Median" = {
           req(is.numeric(df1))
           df_result <- data.frame(Statistic = "Median", 
                                   Value = round(median(df1, na.rm = TRUE), 2))
           },
         
         "Standard Deviation" = {
           req(is.numeric(df1))
           df_result <- data.frame(Statistic="Std Dev", Value=sd(df1, na.rm=TRUE))
           },
         
         # 4.1.1 Univariate graphical summaries
         "Histogram" = {
           req(is.numeric(df1))
           plt <- ggplot(dataset, aes(x = .data[[var1]])) +
             geom_histogram( bins = 30, fill  = "steelblue", color = "white", alpha = 0.85) +
             labs(
               title    = paste("Histogram of", var1),
               subtitle = "Distribution of values",
               x        = var1,
               y        = "Count") +
             theme_minimal(base_size = 14) +
             theme(
               plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
               plot.subtitle    = element_text(size = 12, hjust = 0.5, color = "grey40"),
               axis.title       = element_text(face = "bold"),
               panel.grid.major = element_line(color = "grey90"),
               panel.grid.minor = element_blank()
               )
           },
         
         "Boxplot" = {
           req(is.numeric(df1))
           plt <- ggplot(dataset, aes(x = "", y = .data[[var1]])) +
             geom_boxplot(width = 0.4, fill  = "tomato", color = "black", notch = TRUE,
                          outlier.shape = 21, outlier.fill  = "white", outlier.color = "black",
                          alpha = 0.8) +
             labs(
               title    = paste("Boxplot of", var1),
               subtitle = "Central tendency and dispersion",
               x        = NULL,
               y        = var1) +
             theme_minimal(base_size = 14) +
             theme(
               plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
               plot.subtitle    = element_text(size = 12, hjust = 0.5, color = "grey40"),
               axis.text.x      = element_blank(),
               axis.ticks.x     = element_blank(),
               axis.title.y     = element_text(face = "bold"),
               panel.grid.major = element_line(color = "grey90"),
               panel.grid.minor = element_blank()
             )
           },
         
         # 4.2 Bivariate summaries
         "Contingency Table" = {
           req(var2) # second variable required
           tab <- table(df1, df2, useNA="ifany")
           df_result <- as.data.frame.matrix(tab)
           },

         "Bivariate Scatter" = {
           req(var2)
           plt <- ggplot(dataset, aes(x = .data[[var1]], y = .data[[var2]])) +
             geom_point(shape = 21, size  = 2.5, alpha = 0.7, fill  = "steelblue", 
                        color = "white" ) +
             geom_smooth(
               method = "lm", se= TRUE, linetype = "dashed", linewidth = 0.8, 
               color = "tomato", fill = "tomato", alpha = 0.2 ) +
             labs(
               title    = paste("Scatter:", var1, "vs", var2),
               subtitle = "With linear fit and 95% CI",
               x        = var1,
               y        = var2 ) +
             theme_minimal(base_size = 14) +
             theme(
               plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
               plot.subtitle    = element_text(size = 12, hjust = 0.5, color = "grey40"),
               axis.title       = element_text(face = "bold"),
               panel.grid.major = element_line(color = "grey90"),
               panel.grid.minor = element_blank()
             )
           },
         
         # 4.3 Multivariate summary
         "Missingness Summary" = {
           # percent missing per column
           miss_pct <- sapply(dataset, function(col)
             100 * sum(is.na(col))/length(col))
           df_result <- data.frame(Variable=names(miss_pct), MissPct=miss_pct)
           },
         
         {
           # 4.4 Unknown method
           showModal(modalDialog(
             title = "Unknown method",
             paste0("Method '", method, "' is not implemented."),
             easyClose = TRUE
             ))
           return(NULL)
           }
         )
  
  # 5. Return values in a list
  list(
    method    = method,
    variable  = var1,
    variable2 = var2,
    result    = df_result,
    plot      = plt
    )
  
  }

## Refresh analysis ----

refresh_all_analyses <- function(current_dataset, analysis_list) {
  lapply(analysis_list, function(anal) {
    updated <- perform_analysis(current_dataset, anal$method, anal$variable, anal$variable2)
    anal$result <- updated$result
    anal$plot   <- updated$plot
    anal  # conserva id, label, etc.
  })
}

## Dashboard box ---- 

dashboard_box <- function(itm, anal_item) {
  
  # 1. Unique box identifier (used for output IDs)
  box_id <- itm$id
  delete_btn_id <- paste0("delete_container_", box_id)
  
  # 2. Styling each box
  title_tag <- div(
    style = "display: flex; justify-content: space-between; align-items: center;",
    
    # title
    span(style = "font-weight: bold; font-size: 16px;", anal_item$label)
    )
  
  # body
  inner_ui <- if (itm$view == "Table")
    DTOutput(paste0("out_", box_id))
  else
    plotOutput(paste0("out_", box_id), height = "300px")
  
  # 3. Wrap everything in a container so we can remove or hide it later by ID
  div(
    id = paste0("wrapper_", box_id),
    
    # shinydashboard::box container
    box(
      id = box_id,
      class = "dynamic-box",
      title = title_tag,
      status = "primary",
      solidHeader = TRUE,
      width = NULL,
      
      # inner UI element (either a table or a plot)
      inner_ui
      )
    )
  }


## Rebuild dashboard ----

rebuild_dashboard_ui <- function(items, analysis_list, dashboard_items_rv, 
                                 input, output, session) {
  
  # 0. Show all placeholders
  for (slot in seq_len(4)) {
    shinyjs::show(selector = paste0("#ph_", slot))
  }
  
  # 1. Remove any previously inserted dashboard boxes (whose id starts with "wrapper_" )
  removeUI(selector = "div[id^='wrapper_']", multiple = TRUE) 
  
  # 2. Loop over every item that should remain in the dashboard
  for (itm in items) {
    anal_item <- analysis_list[[ itm$source_idx ]]
    
    # 2.1 Insert the UI 
    insertUI(
      selector = paste0("#ph_", itm$slot),
      where    = "afterEnd",
      ui       = dashboard_box(itm, anal_item)
      )
    
    # 2.2 Hook up the correct render function
    if (itm$view == "Table") {
      local({
        sel_idx <- itm$source_idx
        out_id  <- paste0("out_", itm$id)
        output[[out_id]] <- renderDT({
          datatable(
            analysis_list[[sel_idx]]$result,
            options  = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
            )
          })
        })
      
      } else {
        
        local({
        sel_idx <- itm$source_idx
        out_id  <- paste0("out_", itm$id)
        output[[out_id]] <- renderPlot({
          p <- analysis_list[[sel_idx]]$plot
          if (!is.null(p)) {
            
            # if perform_analysis returned a ggplot object, just print it
            print(p) 
            
            } else {
            
            # otherwise fall back to a base‐R barplot 
            vals <- analysis_list[[sel_idx]]$result
            labs <- if (analysis_list[[sel_idx]]$method == "Frequency") vals$Value else names(vals)
            
            barplot(
              vals$Frequency,
              names.arg = labs,
              las       = 2,
              main      = analysis_list[[sel_idx]]$label
              )
            }
          })
        })
        }
    
    # 2.3.  Hide the placeholder
    shinyjs::hide(selector = paste0("#ph_", itm$slot))
    }
  }


## Valid variables for each analysis ----

get_valid_vars <- function(data, method) {
  switch(method,
         
         # 1. Univariate methods that require numeric inputs:
         "Mean"                = names(data)[sapply(data, is.numeric)],
         "Median"              = names(data)[sapply(data, is.numeric)],
         "Standard Deviation"  = names(data)[sapply(data, is.numeric)],
         "Histogram"           = names(data)[sapply(data, is.numeric)],
         "Boxplot"             = names(data)[sapply(data, is.numeric)],
         
         # 2.  Frequency tables accept only categorical (factor or character) inputs:
         "Frequency"           = names(data)[
           sapply(data, function(x) is.factor(x) || is.character(x))],
         
         # 3. Contingency tables require both variables to be categorical:
         "Contingency Table"   = names(data)[
           sapply(data, function(x) is.factor(x) || is.character(x))],
         
         # 4. Bivariate scatterplots require numeric variables:
         "Bivariate Scatter"   = names(data)[sapply(data, is.numeric)],
         
         # 5. Missingness summary can use all columns:
         "Missingness Summary" = names(data),
         
         # Default: if the method isn’t one of the above, return all column names
         names(data)
         )
  }


# Data storage ----

projectData <- reactiveVal(list()) # relative data storage



