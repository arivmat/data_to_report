## Andrea Rivera Mateos
## Date: June 2025
## Project: From Data to Report

ui <- dashboardPage(
  
  # Header ----
  dashboardHeader(
    title = tags$div(
      HTML('<span style="font-size: 20px; font-weight: bold;">D2R</span>
            <span style="font-size: 10px; margin-left: 5px; font-weight: normal;">v1.0</span>')
    ),
    tags$li(
      class = "dropdown",
      style = "margin-right: 20px; margin-top: 8px;",
      actionButton("save_full_progress",
                   label = tagList(icon("save"), "Save project"),
                   style = "background-color: #003247; color: white; border:none;")
      )
    ),
  
  #  Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Home",          tabName = "home",           icon = icon("home")),
      menuItem("Data Cleaning", tabName = "data_cleaning",  icon = icon("table")),
      menuItem("Data Analysis", tabName = "data_analysis",  icon = icon("chart-bar")),
      menuItem("Dashboard",     tabName = "dashboard_view", icon = icon("tachometer-alt")),
      menuItem("Export",        tabName = "export",         icon = icon("file-export"))
      )
    ),
  
  # Body ----
  dashboardBody(
    useShinyjs(),
    
    ## Custom CSS ----
    tags$head(tags$style(HTML("
      
      /* ===========================
       Base styles
      =========================== */
      body, .box {
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      .content-wrapper,
      .right-side {
        background-color: #FAFAFA;
      }
      .main-header .navbar {
        background-color: #FFFFFF;
        border-bottom: 1px solid #DDDDDD;
      }
      .main-header .logo {
        background-color: #FFFFFF;
        color: #333333;
        font-weight: bold;
      }
      
      /* ===========================
         Sidebar
      =========================== */
      .main-sidebar {
        background-color: #EFEFEF !important;
      }
      .main-sidebar .sidebar a {
        color: #555555;
      }
      .main-sidebar .sidebar .active > a {
        background-color: #1F77B4 !important;
        color: #FFFFFF !important;
      }
      
      /* ===========================
         Box styling
      =========================== */
      .box {
        border-top-color: #94B4C1;
        box-shadow: none;
      }
      .box-primary > .box-header {
        background-color: #FFFFFF;
        border-bottom: 1px solid #DDDDDD;
      }
      .box-primary .box-title {
        color: #FFFFFF !important;
      }
      
      /* ===========================
         Buttons
      =========================== */
      .btn {
        border-radius: 4px;
        font-weight: 500;
      }
      .btn-primary {
        background-color: #E2A269;
        border-color: #E2A269;
      }
      .btn-primary:hover {
        background-color: #C17A3C;
      }
      
      #main_buttons {
        display: flex !important;
        justify-content: center;
        gap: 40px;
        margin: 80px 0 20px 0 !important;
      }
      #button1,
      #button2 {
        font-size: 18px !important;
        padding: 15px 40px !important;
        width: 220px !important;
        height: 60px !important;
      }
      
      #save_analysis {
        background-color: #A59E5D !important;
        border-color: #A59E5D !important;
        color: #FFFFFF !important;
      }
      #save_analysis:hover {
        background-color: #8A9452 !important;
      }
      
      /* ===========================
         Data Cleaning buttons
      =========================== */
      .tabItem[data-value='data_cleaning'] .btn-default {
        font-size: 12px !important;
        padding: 5px 10px !important;
      }
      
      .tabItem[data-value='data_cleaning'] .data-clean-btns .btn {
        border-radius: 4px !important;
        border: 1px solid #ccc !important;
        background-color: #f7f9fc !important;
        color: #333 !important;
        text-align: left !important;
        padding: 8px 12px !important;
        margin-bottom: 6px !important;
        font-size: 13px !important;
      }
      .tabItem[data-value='data_cleaning'] .data-clean-btns .btn:hover {
        background-color: #e6efff !important;
      }
      .tabItem[data-value='data_cleaning'] .data-clean-btns .btn .fa {
        margin-right: 8px;
      }
      .tabItem[data-value='data_cleaning'] .data-clean-btns #undo_edit {
        background-color: #003247 !important;
        border-color: #003247 !important;
        color: #FFFFFF !important;
      }
      .tabItem[data-value='data_cleaning'] .data-clean-btns #undo_edit:hover {
        background-color: #165a9c !important;
      }
      .tabItem[data-value='data_cleaning'] .data-clean-btns #save_edits {
        margin-bottom: 20px !important;
      }
      
      /* ===========================
         Form Container
      =========================== */
      #formContainer {
        display: none;
        position: absolute;
        top: 70px;
        left: 50%;
        transform: translateX(-50%);
        width: 600px;
        background: #FFFFFF;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.3);
        z-index: 2000;
      }
      #formContainer .box-header {
        background-color: #1F77B4 !important;
        color: #FFFFFF !important;
      }
      #formContainer .box {
        border-top-color: #1F77B4 !important;
      }
      
      #undo_edit {
        float: right;
        margin-bottom: 10px;
      }
      
      /* ===========================
         Placeholders
      =========================== */
      .placeholder-box {
        border: 2px dashed #CCCCCC;
        background-color: #FFFFFF;
        min-height: 200px;
        position: relative;
      }
      .placeholder-plus {
        color: #BBBBBB;
        font-weight: bold;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        font-size: 32px;
      }

    "))),
    
    
    ## Tabs ----
    tabItems(
      
      ### 1. Home ----
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            div(
              id = "main_buttons",
              style = "display:flex; justify-content:center; gap:40px; margin:20px 0;",
              
              actionButton(
                "button1", # new project button
                tagList(icon("folder-plus"), "New project"),
                style = "background-color: #E2A269; border-color: #E2A269; color: white;"
                ),
              
              actionButton(
                "button2", # previous project button
                tagList(icon("folder-open"), "Previous project"),
                style = "background-color: #A59E5D; border-color: #A59E5D; color: white;"
                )
              )
            )
          )
        ),
      
      
      ### 2. Data cleaning ----
      tabItem(
        tabName = "data_cleaning",
        
        #### 2.1 data summary ----
        fluidRow(
          box(
            title       = "Data Summary",
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            DT::DTOutput("data_summary_clean")
            )
          ),
        
        #### 2.2 main row ----
        fluidRow(
          
          ##### 2.2.1 select variables & tools ----
          column(
            width = 4,
            box(
              title       = "Select Variables & Tools",
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              # deselect all + checklist de variables
              div(style = "text-align: right; margin-bottom: 10px;",
                  actionButton("deselect_all", "Deselect All", class = "btn btn-default")
                  ),
              checkboxGroupInput("variables", label = NULL, choices = NULL, selected = NULL),
              hr(),
              
              # toggle preview vs edit
              checkboxInput("edit_mode", "Edit data", value = FALSE),
              hr(),
              
              # edit-mode buttons
              conditionalPanel(
                "input.edit_mode == true",
                div(class = "btn-group-vertical data-clean-btns",
                    actionButton("undo_edit", # undo last action      
                                 label = tagList(icon("undo"),"Undo Last Action"),
                                 class="btn btn-info btn-block"),
                    
                    actionButton("rename_column", # rename column
                                 label = tagList(icon("font"), "Rename Column"),
                                 class="btn btn-default btn-block"),
                    
                    actionButton("drop_na_rows", # drop na
                                 label = tagList(icon("trash-alt"), "Drop rows with NA"),  
                                 class="btn btn-default btn-block"),
                    
                    actionButton("convert_factors", # convert into factor
                                 label = tagList(icon("tags"), "Convert to factors"), 
                                 class="btn btn-default btn-block"),
                    
                    actionButton("fill_na_unknown", # fill na with 'unknown'
                                 label = tagList(icon("question-circle"), "Fill NAs → 'Unknown'"), 
                                 class="btn btn-default btn-block"),
                    
                    actionButton("fill_na_mean", # fill na with mean
                                 label = tagList(icon("calculator"), "Fill NAs → Mean"),   
                                 class="btn btn-default btn-block"),
                    
                    actionButton("parse_dates", # change dates    
                                 label = tagList(icon("calendar-alt"), "Parse Dates"),
                                 class="btn btn-default btn-block"),
                    
                    actionButton("recode_values",  # recode values
                                 label = tagList(icon("exchange-alt"),"Recode Values"),      
                                 class="btn btn-default btn-block"),
                    
                    actionButton("save_edits", # save changes
                                 label = tagList(icon("save"), "Save Changes"), 
                                 class = "btn btn-primary btn-block", style = "margin-top: 16px;")
                    )
                )
              )
            ),
          
          ##### 2.2.2 data preview ----
          column(
            width = 8,
            box(
              title       = "Data View",
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              
              # show either DTOutput or rhandsontable
              conditionalPanel(
                condition = "input.edit_mode == false", 
                DTOutput("tablePreview")), # data table preview

              conditionalPanel(
                condition = "input.edit_mode == true", 
                rHandsontableOutput("editable_table", height = "600px") # edit data table preview
                )
              )
            )
          )
        ),
      
      
      ### 3. Data analysis ----
      tabItem(
        tabName = "data_analysis",
        
        fluidRow(
          
          #### 3.1 analysis selection ----
          box(
            title    = "Analysis",
            status   = "primary",
            solidHeader = TRUE,
            width    = 4,

            selectInput("analysis_method", "Method:", 
                        choices = c(
                          "Frequency", "Mean", "Median", "Standard Deviation",
                          "Histogram", "Boxplot", 
                          "Contingency Table", 
                          "Missingness Summary",
                          "Bivariate Scatter"
                          )),
            selectInput("analysis_var1", "Variable 1:", choices = NULL),
            uiOutput("var2_ui"), # appears only for bivariate methods
            
            div(
              style = "display:flex; gap:10px; margin-top:10px;",
              actionButton("run_analysis", "Run Analysis", class = "btn btn-primary"),
              actionButton("save_analysis", "Save Analysis", class = "btn btn-default", disabled = "disabled")
            )
          ),
          
          #### 3.2 results ----
          box(
            title = "Current Result", status = "primary", solidHeader = TRUE, width = 8,
            conditionalPanel(
              condition = "output.has_plot",
              plotOutput("analysis_plot", height = "350px") # plot
              ),
            conditionalPanel(
              condition = "!output.has_plot",
              DTOutput("analysis_table") # table
              )
            )
          ),
        
        #### 3.3 save analysis ----
        fluidRow(
          box(
            width = 12,
            h4("Saved Analyses:"),
            uiOutput("analysis_list")
            )
          )
        ),

      
      ### 4. Dashboard ----
      tabItem(
        tabName = "dashboard_view",
  
        ##### 4.1 "header row": summary + project info ----
        fluidRow(
          
          # summary
          column(
            width = 9,
            box(
              title       = "Data Summary",
              status      = "primary",
              solidHeader = TRUE,
              width       = NULL,
              DTOutput("data_summary")
              )
            ),
          
          # project info
          column(
            width = 3,
            box(
              title       = "Project Info",
              status      = "info",
              solidHeader = TRUE,
              width       = NULL,
              tags$div(
                style = "line-height: 1.8;",
                tags$b("Project Title:"), textOutput("proj_title", inline = TRUE), tags$br(),
                tags$b("Client Name:"),   textOutput("proj_client", inline = TRUE), tags$br(),
                tags$b("Date:"),          textOutput("proj_date", inline = TRUE)
                )
              )
            )
          ),
        
        br(),
        
        #### 4.2 placeholders rows ----
        # 1st row
        fluidRow(
          
          # placeholder 1
          column(
            width = 4,
            div(
              id    = "ph_1",                 
              class = "placeholder-box",
              div(class = "placeholder-plus", "+"),
              actionButton( # invisible button that covers the entire div
                inputId = "add_1",
                label   = "",
                style   = "position:absolute; top:0; left:0; width:100%; height:100%;
                opacity:0; cursor:pointer; z-index: 10;"
                )
              )
            ),
          
          # placeholder 2
          column(
            width = 8,
            div(
              id    = "ph_2",             
              class = "placeholder-box",
              div(class = "placeholder-plus", "+"),
              actionButton(
                inputId = "add_2",
                label   = "",
                style   = "position:absolute; top:0; left:0; width:100%; height:100%;
                opacity:0; cursor:pointer; z-index: 10;"
                )
              )
            )
          ),
        
        br(),
        
        #2nd row
        fluidRow(
          
          # placeholder 3
          column(
            width = 9,
            div(
              id    = "ph_3",                 
              class = "placeholder-box",
              div(class = "placeholder-plus", "+"),
              actionButton(
                inputId = "add_3",
                label   = "",
                style   = "position:absolute; top:0; left:0; width:100%; height:100%;
                opacity:0; cursor:pointer; z-index: 10;"
                )
              )
            ),
          
          # placeholder 4
          column(
            width = 3,
            div(
              id    = "ph_4",                 
              class = "placeholder-box",
              div(class = "placeholder-plus", "+"),
              actionButton(
                inputId = "add_4",
                label   = "",
                style   = "position:absolute; top:0; left:0; width:100%; height:100%;
                opacity:0; cursor:pointer; z-index: 10;
                "
                )
              )
            )
          ),
        
        # This empty div is where new boxes get inserted via insertUI()
        div(id = "dynamic_dashboard")
        
        ),
      

      ### 5. Export (pptx)----

      tabItem(
        tabName = "export",
        fluidRow(
          column(
            width = 8, offset = 2,
            br(), br(),
            h3("Export Dashboard to PowerPoint"),
            downloadButton(
              outputId = "download_ppt_selected",
              label    = "Download PPT",
              class    = "btn btn-primary",
              style    = "font-size:16px; padding:10px 20px;"
              )
            )
          )
        )
      
      )  # end tabItems()
    )   # end dashboardBody()
  )    # end dashboardPage()


