# Load libraries -------------------------------------------------------------

if(!require("shiny")) {
  install.packages("shiny")
  library(shiny)
} 
if(!require("shinyjs")) {
  install.packages("shinyjs")
  library(shinyjs)
} 
if(!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)  
}
if(!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)  
}
if(!require("shinycssloaders")) {
  install.packages("shinycssloaders")
  library(shinycssloaders)
}
if(!require("excel.link")) {
  install.packages("excel.link")
  library(excel.link)  
} 
if(!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
} 
if(!require("skimr")) {
  install.packages("skimr")
  library(skimr)
}
if(!require("DT")) {
  install.packages("DT")
  library(DT)
}
if(!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if(!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
} 
if(!require("plotly")) {
  install.packages("plotly")
  library(plotly)
} 
if(!require("leafem")) {
  install.packages("leafem")
  library(leafem)
} 
if(!require("stringr")) {
  install.packages("stringr")
  library(stringr)
} 
if(!require("sf")) {
  install.packages("sf")
  library(sf)
} 
# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),
  br(), 
  
  ## Ribbon ----
  fluidRow(
    useSweetAlert(),
    column(4, div(id = "img-id", img(src = "logos.png"), style = {"padding-left:20px"})),
    column(3, uiOutput("file_select")),
    column(3, uiOutput("pword_input"),
           actionButton("enter", "Enter", icon = icon("check"), class="btn btn-primary btn-sm")),
    column(2,br(),actionBttn("close","Close", style = "simple", color = "danger", icon = icon("power-off"))),
  ),
  br(),
  navbarPage("Teknaf Data Visualiser",
             
             ## Documentation ----
             tabPanel("Instructions",
                      column(
                        width = 3,
                        uiOutput("doc_select")
                      ),
                      column(
                        width = 9,
                        tags$style(".img {
                            margin-top:-35px;
                          }"),
                        div(class="img",withSpinner(imageOutput("doc_image"),color="#00B4FF"))
                      )),
             
             ## Survey Qs ----
             tabPanel("Survey Questions",
                      fluidRow(div(sidebarPanel(
                        width=3,
                        uiOutput("q_select")
                      ),
                      mainPanel(withSpinner(dataTableOutput("filtered_Qdata"),color="#00B4FF"))
                      ))
             ),
             
             ## Summary Stats ----
             tabPanel("Summary Stats",
                      sidebarPanel(
                        width = 3,
                        id = "stat_sidebar",
                        uiOutput("choose_tables"),
                        conditionalPanel("input.choose_tables=='Categorical'",
                                         uiOutput("var_toggle1")),
                        conditionalPanel("input.choose_tables=='Numeric'",
                                         uiOutput("var_toggle")),
                        uiOutput("group_stats"),
                        conditionalPanel(
                          "input.group_stats != 'None'",
                          withSpinner(plotOutput("sum_plot", width="auto", height = 300),color="#00B4FF")
                        )
                      ),
                      mainPanel(
                        div(id="fac_table",
                            withSpinner(dataTableOutput("sum_factor"),color="#00B4FF")
                        ),
                        br(),
                        div(id="num_table",
                            withSpinner(dataTableOutput("sum_num"),color="#00B4FF")
                        )
                      )
             ),
             
             ## Maps ----
             tabPanel("Map",
                      sidebarPanel(
                        width = 3,
                        id = "map_sidebar",
                        uiOutput("style"),
                        uiOutput("circle_color"),
                        uiOutput('points_select')
                        # conditionalPanel("input.style=='Points'",
                        #       uiOutput("points_select")),
                        # conditionalPanel("input.style=='Heatmap'",
                        #                  uiOutput(""))
                      ),
                      
                      mainPanel(
                        width = 9,
                        tags$style(type = "text/css", "#mymap {height: calc(100vh - 250px) !important;}"),
                        withSpinner(leafletOutput("mymap"),color="#00B4FF"),
                      )
             ),
             
             ## Plots ----
             tabPanel("Plots",
                      tabsetPanel(
                        tabPanel("Bar Plots",
                                 br(),
                                 sidebarPanel(
                                   width=3,
                                   uiOutput("variable_x_bar"),
                                   uiOutput("variable_groupby_bar"),
                                   HTML('<iframe width="100%" height="300" src="https://www.youtube.com/embed/ZAhCJpVFztg" frameborder="0" allowfullscreen></iframe>')
                                 ),
                                 mainPanel(
                                   tags$style(type = "text/css", "#bar_plot {height: calc(100vh - 300px) !important;}"),
                                   withSpinner(plotlyOutput("bar_plot",
                                                            width="100%"),color="#00B4FF")
                                 ),
                        ),
                        tabPanel("Box Plots",
                                 br(),
                                 sidebarPanel(
                                   width=3,
                                   uiOutput("variable_x_box"),
                                   uiOutput("variable_y_box"),
                                   uiOutput("variable_groupby_box"),
                                   HTML('<iframe width="100%" height="300" src="https://www.youtube.com/embed/JStltOK6uqQ" frameborder="0" allowfullscreen></iframe>')
                                 ),
                                 mainPanel(
                                   tags$style(type = "text/css", "#box_plot {height: calc(100vh - 300px) !important;}"),
                                   withSpinner(plotlyOutput("box_plot",
                                                            width="100%"),color="#00B4FF")
                                 )
                        ),
                        tabPanel("Scatter Plots",
                                 br(),
                                 sidebarPanel(
                                   width=3,
                                   uiOutput("variable_x_scat"),
                                   uiOutput("variable_y_scat"),
                                   uiOutput("variable_groupby_scat"),
                                   HTML('<iframe width="100%" height="300" src="https://www.youtube.com/embed/4eDLMU3_zkA" frameborder="0" allowfullscreen></iframe>')
                                 ),
                                 mainPanel(tags$style(type = "text/css", "#scatter_plot {height: calc(100vh - 300px) !important;}"),
                                           withSpinner(plotlyOutput("scatter_plot",
                                                                    width="100%"),color="#00B4FF")
                                 )
                        )
                        
                      )
             ),
             
             ## Data ----
             tabPanel("Data",
                      tabsetPanel(
                        tabPanel("Numeric Data",
                                 fluidRow(
                                   br(),
                                   column(3, actionButton("toggle_sidebar", label = NULL,
                                                          icon = icon("arrow-right-arrow-left"),
                                                          class = "btn-primary",
                                                          width = "200px")
                                   )
                                 ),
                                 br(),
                                 div(id="Sidebar",sidebarPanel(
                                   width=3,
                                   uiOutput("choose_columns"),
                                   dataTableOutput("filter_Q"))
                                 ),
                                 mainPanel(
                                   withSpinner(dataTableOutput("filter_clean"),color="#00B4FF")
                                 )
                                 
                        ),
                        tabPanel("Text Data",
                                 fluidRow(
                                   br(),
                                   column(3, actionButton("toggle_sidebar1", label = NULL,
                                                          icon = icon("arrow-right-arrow-left"),
                                                          class = "btn-primary",
                                                          width = "200px"
                                   )
                                   )
                                 ),
                                 br(),
                                 
                                 div(id="Sidebar1",sidebarPanel(
                                   width=3,
                                   uiOutput("choose_columns1"),
                                   dataTableOutput("filter_Q1")
                                 )
                                 ),
                                 mainPanel(
                                   withSpinner(dataTableOutput("filter_tidy"),color="#00B4FF")
                                 )
                        ),
                        tabPanel("Raw Data",
                                 br(),
                                 withSpinner(dataTableOutput("raw_data"),color="#00B4FF")
                        )
                      )
                      
             ),
  
  )
)

# Server -----------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
  
# Ribbon -----------------------------------------------------------------------
  
  ## UI elements ----------
  
  # file select
  output$file_select <- renderUI({
    fileInput("file","Import File:",buttonLabel = "Select",
              multiple = FALSE, accept = c(".xlsx", ".csv"),placeholder = "No file selected")
  })
  
  # password input
  output$pword_input <-renderUI({
    passwordInput("pword","Password:", "", placeholder = "Enter Excel password")
  })
  
  # close app button
  # confirmation
  observeEvent(input$close, {
    ask_confirmation(
      inputId = "close_conf",
      title = "Close App",
      type = "warning",
      text = "Are you sure you want to quit?",
      btn_labels = c("Cancel","Close"),
      btn_colors = c("#00b4ff","#DC3545")
    )
  })
  
  ## Alerts ----
  
  # close app
  observe({
    req(input$close_conf)
    stopApp()
  })
  
  # password entry
  observeEvent(input$enter, {
    if(inherits(data(), "try-error")) {
      sendSweetAlert(
        session = session,
        title = "Wrong Password",
        type = "error"
      )
      reset(id = "pword", asis = TRUE)
      reset(id = "enter", asis = TRUE)
    } else {
      sendSweetAlert(
        session = session,
        title = "Password Accepted",
        type = "success"
      )
    }
  })
  
  # Data -----------------------------------------------------------------------
  
  ## UI elements ----------
  
  # sidebar toggle - clean
  observeEvent(input$toggle_sidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  
  # column select - clean
  output$choose_columns <- renderUI({
    if (phase() == 1) {
      checkboxGroupInput(
      "varchoices", "Select columns to display:",
      choices = c("Location","Union","Ward","Village","Camp","Block","Sex",
                  "Sociodemographic","Work","Perception of Water Services",
                  "HWISE","Water Access, Quantity, Utility, Stability",
                  "Livelihood Activities","Paying for Water","Well-being"),
      selected = "HWISE" 
    )
    } else if (phase() == 2){
      checkboxGroupInput(
        "varchoices", "Select columns to display:",
        choices = c("Location","Union","Ward","Village","Camp","Block","Sex",
                    "Sociodemographic","Livelihood & Income",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Paying for Water","Well-being"),
        selected = "HWISE" 
      )
    }
  })
  
  # sidebar toggle - tidy
  observeEvent(input$toggle_sidebar1, {
    shinyjs::toggle(id = "Sidebar1")
  })
  
  # column select - tidy
  output$choose_columns1 <- renderUI({
    if (phase() == 1) {
      checkboxGroupInput(
        "varchoices1", "Select columns to display:",
        choices = c("Location","Union","Ward","Village","Camp","Block","Sex",
                    "Sociodemographic","Work","Perception of Water Services",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Livelihood Activities","Paying for Water","Well-being"),
        selected = "HWISE" 
      )
    } else if (phase() == 2){
      checkboxGroupInput(
        "varchoices1", "Select columns to display:",
        choices = c("Location","Union","Ward","Village","Camp","Block","Sex",
                    "Sociodemographic","Livelihood & Income",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Paying for Water","Well-being"),
        selected = "HWISE" 
      )
    }
  })
  
  ## Create data objects
  
  # Read Excel file
  data <- reactive({
    req(input$enter)
    data_file <- input$file
    df <- try(xl.read.file(data_file$datapath,
                           header = FALSE,
                           password =  input$pword))
    return(df)
  })
  
  # Set data phase
  phase <- reactive({
    if (is.na(data()[1,1])) {
      phase = 1
    } else if (data()[1,1] == "UNIQUE-ID") {
      phase = 2
    }
    return(phase)
  })

  
  ### Raw Data ----
  raw.data <- reactive({
    req(data())
    df <- data()
    
    # (DRY) ----
    if (phase()==1){
        # Fill blank row 1 content with row 2
        for (i in 1:length(df[1, ])) {
          ifelse(is.na(df[1, i]), df[1, i] <- df[2,i ], df[1, i])
        }

        # Change all chars to lowercase
        df <- data.frame(sapply(df, tolower))

        # Set row 1 as colnames
        colnames(df) <- df[1, ]

        # Remove row 1 as now used in colnames
        df <- df[-1, ]

        # Append digits to make colnames unique
        colnames(df) <- make.unique(colnames(df), sep = "_")

        return(df)
        
    # (WET) ----
    } else if (phase()==2) {
      # Change all chars to lowercase
      df <- data.frame(sapply(df, tolower))
      
      # Set row 1 as colnames
      colnames(df) <- df[1, ]
      
      # Remove row 1 as now used in colnames
      df <- df[-1, ]
      
      # Append digits to make colnames unique
      colnames(df) <- make.unique(colnames(df), sep = "_")
      
      # Fix name of location column
      df <- df %>% rename("location" = "host/refugee",
                          "sex" = "gender",
                          "gps-longitude" = "gps-long ")
      
      return(df)
      
    } 
  })
  
  ### Clean Data ----
  # *completely numeric except for free text, and factors
  
  num.data <- reactive({
    
    # (DRY) ----
      if (phase()==1) {
        #### Define columns to include
        df <- select(raw.data(),
                     "location",
                     "union",
                     "ward",
                     "village",
                     "camp",
                     "block",
                     "gps-latitude",
                     "gps-longitude",
                     "sex",
                     starts_with(c("sd","w","p","hw","wat","liv","wb")),
                     -c("sd5","sd13_1","sd16_1","sd20_1","w1","wat12",
                        "wat12_6","wat13","wat13_1","wat13_2","wat14_4","wat15",
                        "liv1","liv4")) %>%
          slice(-1)
        
        ### Replace text responses with numeric codes
        
        # True / False
        df[ , ] <- apply(df[ , ], 2, function(x) {
          x = replace(x, which(x=="true"),1)
          x = replace(x, which(x=="false"), 0)
        })
        
        # Replace sd values
        df[, grepl("sd", names(df))] <- apply(df[,grepl("sd", names(df))], 2, function(x){
          x = replace(x, which(x=="myself"|x=="single"|x=="primary_school"|x=="concrete/brick"|x=="owned"|x=="wood"),1)
          x = replace(x, which(x=="my_spouse"|x=="divorced"|x=="secondary_school"|x=="processed_wood"|x=="rented"|x=="gas_bottles"),2)
          x = replace(x, which(x=="an_adult_child"|x=="adult_child"|x=="widowed"|x=="university_college"|x=="wood/canvas/plastic"|x=="allocated/given_by_authorities"),3)
          x = replace(x, which(x=="a_grandparent"|x=="married"|x=="no_formal_education"),4)
        })
        # "other" not consistently recorded
        df[c("sd1","sd4","sd12","sd13")][df[c("sd1","sd4","sd12","sd13")] == "other"] <- 5
        df["sd16"][df["sd16"] == "other"] <- 4
        df["sd20"][df["sd20"] == "other"] <- 3
        
        # Replace "hw" question values
        df[, grepl("hw", names(df))] <- apply(df[, grepl("hw",names(df))], 2, function(x){
          x = replace(x, which(x=="0_times"|x=="announced/scheduled"), 1)
          x = replace(x, which(x=="1-2_times"|x=="unexpected"), 2)
          x = replace(x, which(x=="3-10_times"), 3)
          x = replace(x, which(x=="11-20_times"), 4)
          x = replace(x, which(x=="more_than_20_times"), 5)
          x = replace(x, which(x=="not_applicable"), 88)
          x = replace(x, which(x=="dont know"), 99)
        })
        
        # Replace "wat" question values
        df[, grepl("wat", names(df))] <- apply(df[, grepl("wat",names(df))], 2, function(x){
          x = replace(x, which(x=="no"), 0)
          x = replace(x, which(x=="piped_water_to_dwelling"|x=="yes"), 1)
          x = replace(x, which(x=="stand_pipe"|x=="dont know"), 2)
          x = replace(x, which(x=="borehole/tubewell"), 3)
          x = replace(x, which(x=="protected_dug_well"), 4)
          x = replace(x, which(x=="unprotected_dug_well"), 5)
          x = replace(x, which(x=="protected_sping"), 6)
          x = replace(x, which(x=="unprotected_spring"), 7)
          x = replace(x, which(x=="rainwater_collection"), 8)
          x = replace(x, which(x=="small_water_vendor"), 9)
          x = replace(x, which(x=="tanker_truck"), 10)
          x = replace(x, which(x=="bottled_water"), 11)
          x = replace(x, which(x=="bagged_sachet_water"), 12)
          x = replace(x, which(x=="surface_water_pond_river_lake"), 13)
          x = replace(x, which(x=="other_person"), 14)
          x = replace(x, which(x=="other"), 15)
        })
        
        # Replace "liv" question values
        # * It appears water source scale from "wat" has been incorrectly applied. 
        # * Encoding uses "wat" scale, not "liv" scale
        df[, grepl("liv",names(df))] <- apply(df[, grepl("liv",names(df))], 2, function(x){
          x = replace(x, which(x=="piped_water_to_dwelling"|x=="yes_i_own_the_businesses"), 1)
          x = replace(x, which(x=="stand_pipe"|x=="no_i_work_for_somebody_else"), 2)
          x = replace(x, which(x=="borehole/tubewell"), 3)
          x = replace(x, which(x=="protected_dug_well"), 4)
          x = replace(x, which(x=="unprotected_dug_well"), 5)
          x = replace(x, which(x=="protected_sping"), 6)
          x = replace(x, which(x=="unprotected_spring"), 7)
          x = replace(x, which(x=="rainwater_collection"), 8)
          x = replace(x, which(x=="small_water_vendor"), 9)
          x = replace(x, which(x=="tanker_truck"), 10)
          x = replace(x, which(x=="bottled_water"), 11)
          x = replace(x, which(x=="bagged_sachet_water"), 12)
          x = replace(x, which(x=="surface_water_pond_river_lake"), 13)
          x = replace(x, which(x=="other_person"), 14)
          x = replace(x, which(x=="other"), 15)
          x = replace(x, which(is.na(x)), 88)
        })
        
        # Replace "pay" question values
        df[, grepl("pay", names(df))] <- apply(df[, grepl("pay",names(df))], 2, function(x){
          x=replace(x, which(x=="not_important_at_all"|x=="fixed_cost_per_month"),1)
          x=replace(x, which(x=="neither_imp_nor_unimportant"|x=="fixed_cost_per_amount_"),2)
          x=replace(x, which(x=="somewhat_important"|x=="vc_permonth_on_set_factors"),3)
          x=replace(x, which(x=="very_important"|x=="variable_cost_per_amount_or_use"),4)
          x=replace(x, which(x=="no_fixed_payment_is_made"),5)
          x=replace(x, which(x=="it_is_free"),6)
          x=replace(x, which(x=="dont_know"),7)
        })
        
        # Replace "wb" question values
        df[, grepl("wb", names(df))] <- apply(df[, grepl("wb",names(df))], 2, function(x){
          x = replace(x, which(x=="never"),0)
          x = replace(x, which(x=="very_dissatisfied"|x=="better_health_services"|x=="poor"|x=="almost_never"),1)
          x = replace(x, which(x=="somewhat_dissatisfied"|x=="safe_water_services"|x=="fair"|x=="sometimes"),2)
          x = replace(x, which(x=="somewhat_satisfied"|x=="access_to_education"|x=="good"|x=="fairly_often"),3)
          x = replace(x, which(x=="very_satisfied"|x=="better_communication_with_host_communities"|x=="very_good"|x=="very_often"),4)
          x = replace(x, which(x=="safe_return_to_myanmar"|x=="excellent"),5)
          x = replace(x, which(x=="other"),6)
        })
        
        # Re-code for consistency
        df["sd11_2"][df["sd11_2"] == "ngos"] <- "ngo"
        
        df$camp[grepl("nayapara", df$camp)] <- "Nayapara Refugee Camp"
        df$camp <- sapply(df$camp, function(x){
          x = replace(x, which(x=="24"), "Camp 24")
          x = replace(x, which(x=="25"), "Camp 25")
          x = replace(x, which(x=="26"), "Camp 26")
          x = replace(x, which(x=="27"), "Camp 27")
        })
        
        # Define numeric variables
        df <- df %>%
          mutate(across(-c("location",
                           "union",
                           "ward",
                           "village",
                           "camp",
                           "block",
                           "sex",
                           "sd11_2",
                           "sd11_3",
                           "w8",
                           "wat8",
                           "wat14_3",
                           "wat16",
                           "wat17",
                           "liv5",
                           "liv6"), as.numeric))
        
        # Correct irregular year recording "sd9"
        for(i in 1:nrow(df)) {
          if ((!is.na(df[i,"sd9"])) & (df[i,"sd9"] < 1900)) {df[i,"sd9"] = 2022 - df[i,"sd9"]}
        }
        
        ### Compute new variables
        
        # Overall HWISE score
        df_hw <- df %>%
          select(starts_with("hw"), -("hw2a"))
        
        df$hw <- rowSums(pmin(as.matrix(df_hw - 1), 3))
        rm(df_hw)
        
        df <- df %>% 
          relocate(hw, .before = hw1)
        
        # Time to collect per week
        df <- df %>% 
          mutate(wat5a = (wat4 * wat5) * 7) %>%
          relocate(wat5a, .after = wat5)
        # Change to grouped values
        df$wat5b <- sapply(df$wat5a, function(x){
          x = replace(x, which(x<=100), 1)
          x = replace(x, which(x>100 & x<=200), 2)
          x = replace(x, which(x>200 & x<=300), 3)
          x = replace(x, which(x>300 & x<=400), 4)
          x = replace(x, which(x>400 & x<=500), 5)
          x = replace(x, which(x>500 & x<=600), 6)
          x = replace(x, which(x>600), 7)
        })
        df <- df %>% relocate(wat5b, .after = wat5a)
        
        # Collapse boy/girl under 18 to single var
        df <- df %>% 
          mutate(sd6_ = sd6 + sd6_1) %>%
          relocate(sd6_, .before = sd6) %>%
          select(-c(sd6, sd6_1))
        
        ### Collapse multiple choice binary questions
        
        ## sd5
        # collapse to sd5_ * binaries dropped
        df <- df %>%
          mutate(
            sd5_ = case_when(
              sd5_6 == 1 ~ 6,
              sd5_1 == 1 & sd5_2 == 0 & sd5_3 == 0 & sd5_4 == 0 & sd5_5 == 0 & sd5_6 ==0 ~ 1,
              sd5_1 == 0 & sd5_2 == 1 & sd5_3 == 0 & sd5_4 == 0 & sd5_5 == 0 & sd5_6 ==0 ~ 2,
              sd5_1 == 0 & sd5_2 == 0 & sd5_3 == 1 & sd5_4 == 0 & sd5_5 == 0 & sd5_6 ==0 ~ 3,
              sd5_1 == 0 & sd5_2 == 0 & sd5_3 == 0 & sd5_4 == 1 & sd5_5 == 0 & sd5_6 ==0 ~ 4,
              sd5_1 == 0 & sd5_2 == 0 & sd5_3 == 0 & sd5_4 == 0 & sd5_5 == 1 & sd5_6 ==0 ~ 5,
              TRUE ~ 6)
          ) %>%
          relocate(sd5_, .before = sd5_1) %>%
          select(-c(sd5_1, sd5_2, sd5_3, sd5_4, sd5_5, sd5_6))
        
        ## wat12
        # collapse in sd12_ * binaries retained
        df <- df %>%
          mutate(
            wat12_ = case_when(
              wat12_1 == 1 & wat12_2 == 0 & wat12_3 == 0 & wat12_4 == 0 & wat12_5 == 0 ~ 1,
              wat12_1 == 0 & wat12_2 == 1 & wat12_3 == 0 & wat12_4 == 0 & wat12_5 == 0 ~ 2,
              wat12_1 == 0 & wat12_2 == 0 & wat12_3 == 1 & wat12_4 == 0 & wat12_5 == 0 ~ 3,
              wat12_1 == 0 & wat12_2 == 0 & wat12_3 == 0 & wat12_4 == 1 & wat12_5 == 0 ~ 4,
              wat12_1 == 0 & wat12_2 == 0 & wat12_3 == 0 & wat12_4 == 0 & wat12_5 == 1 ~ 0,
              TRUE ~ 5
            )
          ) %>%
          relocate(wat12_, .before = wat12_1)
        # Create binary wat12_6 "combined methods"
        df <- df %>%
          mutate(
            wat12_6_ = case_when(
              wat12_ == 5 ~ 1,
              TRUE ~ 0
            )
          ) %>%
          relocate(wat12_6_, .after = wat12_5)
        
        ## wat15
        # collapse into wat15_ * binaries retained
        for(i in 1:nrow(df)) {
          if(df[i, "wat15_1"] == 1 & rowSums(df[i, c("wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 1}
          else if(df[i, "wat15_2"] == 1 & rowSums(df[i, c("wat15_1","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 2}
          else if(df[i, "wat15_3"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 3}
          else if(df[i, "wat15_4"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 4}
          else if(df[i, "wat15_5"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 5}
          else if(df[i, "wat15_6"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 6}
          else if(df[i, "wat15_7"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 7}
          else if(df[i, "wat15_8"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_9","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 8}
          else if(df[i, "wat15_9"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_10","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 9}
          else if(df[i, "wat15_10"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_11","wat15_12")]) == 0) {df[i, "wat15_"] = 10}
          else if(df[i, "wat15_11"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_12")]) == 0) {df[i, "wat15_"] = 11}
          else if(df[i, "wat15_12"] == 1 & rowSums(df[i, c("wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6","wat15_7","wat15_8","wat15_9","wat15_10","wat15_11")]) == 0) {df[i, "wat15_"] = 12}
          else {df[i, "wat15_"] = 13}
        }
        df <- df %>%
          relocate(wat15_, .before = wat15_1)
        
        ## liv4
        # Collapse into liv4_ * binaries dropped
        df <- df %>%
          mutate(
            liv4_ = case_when(
              liv4_1 == 0 & liv4_2 == 0 & liv4_3 == 1 ~ 0,
              liv4_1 == 1 & liv4_2 == 0 & liv4_3 == 0 ~ 1,
              liv4_1 == 0 & liv4_2 == 1 & liv4_3 == 0 ~ 2,
              TRUE ~ 3
            )
          ) %>%
          relocate(liv4_, .before = liv4_1) %>%
          select(-c(liv4_1, liv4_2, liv4_3))
        
        ## liv1
        # Collapse into liv1_ * binaries retained
        df <- df %>%
          mutate(
            liv1_ = case_when(
              liv1_1 == 1 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 0  ~  1,
              liv1_1 == 0 & liv1_2 == 1 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 0  ~  2,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 1 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 0  ~  3,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 1 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 0  ~  4,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 1 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 0  ~  5,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 1 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 0  ~  6,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 1 & liv1_8 == 0 & liv1_9 == 0  ~  7,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 1 & liv1_9 == 0  ~  8,
              liv1_1 == 0 & liv1_2 == 0 & liv1_3 == 0 & liv1_4 == 0 & liv1_5 == 0 & liv1_6 == 0 & liv1_7== 0 & liv1_8 == 0 & liv1_9 == 1  ~  0,
              TRUE ~ 9
            )
          ) %>%
          relocate(liv1_, .before = liv1_1) 
        # create binary liv1_10_ "multiple income sources"
        df <- df %>%
          mutate(
            liv1_10_ = case_when(
              liv1_ == 9 ~ 1,
              TRUE ~ 0
            )
          ) %>% 
          relocate(liv1_10_, .after = liv1_9)
        
        ### Replace NA vals
        df[is.na(df)] <- "NA"
        
        return(df)
      
    # (WET) ----
      } else if (phase()==2) {
        #### Define columns to include
        df <- select(raw.data(),
                     "location",
                     "union",
                     "ward",
                     "village",
                     "camp",
                     "block",
                     "gps-latitude",
                     "gps-longitude",
                     "sex",
                     starts_with(c("sd","li","wb","wat","hw","pay")),
                     -c("sd5","sd12_1","sd15_1","sd19_1","wb3_1","wat1a"," "," _1",
                        "wat15","wat16","wat17","wat25","wat26")) %>%
        slice(-1)
        
        ### Replace text responses with numeric codes
        
        # Consistent values
        df[ , ] <- apply(df[ , ], 2, function(x) {
          x = replace(x, which(x=="false"|x=="no"), 0)
          x = replace(x, which(x=="true"|x=="yes"),1)
          x = replace(x, which(x=="other"),77)
          x = replace(x, which(x=="dont_know"),88)
        })
        
        # Replace sd values
        df[, grepl("sd", names(df))] <- apply(df[,grepl("sd", names(df))], 2, function(x){
          x = replace(x, which(x=="self"|x=="myself"|x=="single"|x=="no_formal_education"|x=="concrete/brick"|x=="owned"|x=="wood"),1)
          x = replace(x, which(x=="spouse"|x=="my_spouse"|x=="divorced"|x=="primary_school"|x=="processed_wood"|x=="rented"|x=="gas_bottles"),2)
          x = replace(x, which(x=="an_adult_child"|x=="adult_child"|x=="widowed"|x=="boy_children"|x=="secondary_school"|x=="wood/canvas/plastic"|x=="allocated/given_by_authorities"),3)
          x = replace(x, which(x=="a_grandparent"|x=="married"|x=="girl_children"|x=="polytechnic_training"),4)
          x = replace(x, which(x=="other_family_members"|x=="university_college"),5) 
          x = replace(x, which(x=="shared_responsibility"),6)
        })
        
        # Replace "hw" question values
        df[, grepl("hw", names(df))] <- apply(df[, grepl("hw",names(df))], 2, function(x){
          x = replace(x, which(x=="0_times"|x=="announced/scheduled"), 1)
          x = replace(x, which(x=="1-2_times"|x=="unexpected"), 2)
          x = replace(x, which(x=="3-10_times"), 3)
          x = replace(x, which(x=="11-20_times"), 4)
          x = replace(x, which(x=="more_than_20_times"), 5)
        })
        
        # Replace "wat" question values
        df[, grepl("wat", names(df))] <- apply(df[, grepl("wat",names(df))], 2, function(x){
          x = replace(x, which(x=="piped_water_to_dwelling"|x=="january"), 1)
          x = replace(x, which(x=="stand_pipe"|x=="february"), 2)
          x = replace(x, which(x=="borehole/tubewell"|x=="march"), 3)
          x = replace(x, which(x=="protected_dug_well"|x=="april"), 4)
          x = replace(x, which(x=="unprotected_dug_well"|x=="may"), 5)
          x = replace(x, which(x=="protected_sping"|x=="june"), 6)
          x = replace(x, which(x=="unprotected_spring"|x=="july"), 7)
          x = replace(x, which(x=="rainwater_collection"|x=="august"), 8)
          x = replace(x, which(x=="small_water_vendor"|x=="september"), 9)
          x = replace(x, which(x=="tanker_truck"|x=="october"), 10)
          x = replace(x, which(x=="bottled_water"|x=="november"), 11)
          x = replace(x, which(x=="bagged_sachet_water"|x=="december"), 12)
          x = replace(x, which(x=="surface_water_pond_river_lake"), 13)
          x = replace(x, which(x=="other_person"), 14)
        })
        
        # Replace "li" question values
        df[, grepl("li",names(df))] <- apply(df[, grepl("li",names(df))], 2, function(x){
          x = replace(x, which(x=="none"), 1)
          x = replace(x, which(x=="some"), 2)
          x = replace(x, which(x=="about_half"|x=="half"), 3)
          x = replace(x, which(x=="almost"), 4)
          x = replace(x, which(x=="all"), 5)
        })
        
        # Replace "wb" question values
        df[, grepl("wb", names(df))] <- apply(df[, grepl("wb",names(df))], 2, function(x){
          x = replace(x, which(x=="never"),0)
          x = replace(x, which(x=="very_dissatisfied"|x=="better_health_services"|x=="poor"|x=="almost_never"),1)
          x = replace(x, which(x=="somewhat_dissatisfied"|x=="safe_water_services"|x=="fair"|x=="sometimes"),2)
          x = replace(x, which(x=="somewhat_satisfied"|x=="access_to_education"|x=="good"|x=="fairly_often"),3)
          x = replace(x, which(x=="very_satisfied"|x=="better_comms_with_host"|x=="very_good"|x=="very_often"),4)
          x = replace(x, which(x=="safe_return_to_myanmar"|x=="excellent"),5)
          x = replace(x, which(x=="other"),6)
        })
        
        # Replace "pay" question values
        df[, grepl("pay", names(df))] <- apply(df[, grepl("pay",names(df))], 2, function(x){
          x=replace(x, which(x=="not_important_at_all"|x=="fixed_cost_per_month"|x=="dphe"|x=="mobile_phone_or_digital_payment"),1)
          x=replace(x, which(x=="neither_imp_nor_unimportant"|x=="fixed_cost_per_amount_"|x=="union_council"|x=="cash_to_collector"),2)
          x=replace(x, which(x=="somewhat_important"|x=="vc_permonth_on_set_factors"|x=="service_provider"|x=="visit_office_to_pay_cash"),3)
          x=replace(x, which(x=="very_important"|x=="variable_cost_per_amount_or_use"|x=="landowner"),4)
          x=replace(x, which(x=="no_fixed_payment_is_made"|x=="nobody_its_free"),5)
          x=replace(x, which(x=="it_is_free"),6)
          x=replace(x, which(x=="dont_know"),7)
        }) 
        
        # Re-code for consistency
        df$camp <- sapply(df$camp, function(x){
          x = replace(x, which(x=="nayapara registered camp"), "nrc")
        })
        
        df[,c("pay2","pay9")] <- suppressWarnings(sapply(df[,c("pay2","pay9")], as.numeric))
        df$pay9[is.na(df$pay9)] <- 0
        
        # Define numeric variables
        df <- df %>%
          mutate(across(-c("location",
                           "union",
                           "ward",
                           "village",
                           "camp",
                           "block",
                           "sex",
                           "sd10_1",
                           "sd10_2",
                           "li8",
                           "li8_1",
                           "wb9a",
                           "wb9b",
                           "wb9c",
                           "wat2a",
                           "wat8",
                           "wat9_5",
                           "wat28a",
                           "wat28b",
                           "wat29",
                           "pay4_1",
                           "pay5_1"), as.numeric))
        
        ### Compute new variables
        
        # Overall HWISE score
        df_hw <- df %>%
          select(starts_with("hw"), -("hw2a"))
        df$hw <- rowSums(pmin(as.matrix(df_hw - 1), 3))
        rm(df_hw)
        df <- df %>% 
          relocate(hw, .before = hw1)
        
        # Time to collect per week
        df <- df %>% 
          mutate(wat5a = (wat4 * wat5) * 7) %>%
          relocate(wat5a, .after = wat5)
        # Change to grouped values
        df$wat5b <- sapply(df$wat5a, function(x){
          x = replace(x, which(x<=100), 1)
          x = replace(x, which(x>100 & x<=200), 2)
          x = replace(x, which(x>200 & x<=300), 3)
          x = replace(x, which(x>300 & x<=400), 4)
          x = replace(x, which(x>400 & x<=500), 5)
          x = replace(x, which(x>500 & x<=600), 6)
          x = replace(x, which(x>600), 7)
        })
        df <- df %>% relocate(wat5b, .after = wat5a)
        
        # pay 2
        df$pay2[is.na(df$pay2)] <- 0
        df$pay2a <- sapply(df$pay2, function(x){
          x = replace(x, which(x<=100), 0)
          x = replace(x, which(x>100 & x<=200), 1)
          x = replace(x, which(x>200 & x<=300), 2)
          x = replace(x, which(x>300 & x<=400), 3)
          x = replace(x, which(x>400 & x<=500), 4)
          x = replace(x, which(x>500 & x<=600), 5)
          x = replace(x, which(x>600 & x<=700), 6)
          x = replace(x, which(x>700 & x<=800), 7)
          x = replace(x, which(x>800 & x<=900), 8)
          x = replace(x, which(x>900), 9)
        })
        df <- df %>% relocate(pay2a, .after = pay2)
        
        # Merge boy/girl under 18 to single var
        df <- df %>% 
          mutate(sd6_ = sd6 + sd6_1) %>%
          relocate(sd6_, .before = sd6) %>%
          select(-c(sd6, sd6_1))
        
        ### Collapse multiple choice binary questions
        
        ## sd5
        # collapse to sd5_ * binaries dropped
        df <- df %>%
          mutate(
            sd5_ = case_when(
              sd5_6 == 1 ~ 6,
              sd5_1 == 1 & sd5_2 == 0 & sd5_3 == 0 & sd5_4 == 0 & sd5_5 == 0 & sd5_6 ==0 ~ 1,
              sd5_1 == 0 & sd5_2 == 1 & sd5_3 == 0 & sd5_4 == 0 & sd5_5 == 0 & sd5_6 ==0 ~ 2,
              sd5_1 == 0 & sd5_2 == 0 & sd5_3 == 1 & sd5_4 == 0 & sd5_5 == 0 & sd5_6 ==0 ~ 3,
              sd5_1 == 0 & sd5_2 == 0 & sd5_3 == 0 & sd5_4 == 1 & sd5_5 == 0 & sd5_6 ==0 ~ 4,
              sd5_1 == 0 & sd5_2 == 0 & sd5_3 == 0 & sd5_4 == 0 & sd5_5 == 1 & sd5_6 ==0 ~ 5,
              TRUE ~ 6)
          ) %>%
          relocate(sd5_, .before = sd5_1) %>%
          select(-c(sd5_1, sd5_2, sd5_3, sd5_4, sd5_5, sd5_6))
        
        ## wat9
        # collapse in wat9_ * binaries dropped
        df <- df %>%
          mutate(
            wat9_ = case_when(
              wat9 == 1 & wat9_1 == 0 & wat9_2 == 0 & wat9_3 == 0 & wat9_4 == 0 ~ 1,
              wat9 == 0 & wat9_1 == 1 & wat9_2 == 0 & wat9_3 == 0 & wat9_4 == 0 ~ 2,
              wat9 == 0 & wat9_1 == 0 & wat9_2 == 1 & wat9_3 == 0 & wat9_4 == 0 ~ 3,
              wat9 == 0 & wat9_1 == 0 & wat9_2 == 0 & wat9_3 == 1 & wat9_4 == 0 ~ 77,
              wat9 == 0 & wat9_1 == 0 & wat9_2 == 0 & wat9_3 == 0 & wat9_4 == 1 ~ 0,
              TRUE ~ 4
            )
          ) %>%
          relocate(wat9_, .before = wat9_1) %>%
          select(-c(wat9,wat9_1,wat9_2,wat9_3,wat9_4))
        
        ### Replace NA vals
        df[is.na(df)] <- "NA"
        
        return(df)
      }
  })
  
  ### Questions Vector ----
  #A vector containing all questions
  
  questions_vec <- reactive ({
    df <-raw.data()[1, ]
    
    # (DRY) ----
    if (phase() == 1) {
      # Match cols to num.data()
      df <- df[, colnames(df) %in% colnames(num.data())]
      
      # String formatting
      df <- df %>% mutate(across(everything(), 
                                 ~ str_replace(., "income_source_past_12_months_", "income_source:_") %>%
                                   str_replace("primary_way_household_treats_drinking_water","drinking_water_treatment:") %>%
                                   str_replace("your_household_experience_the_fewest_water_problems", "fewest_water_problems:") %>%
                                   str_replace("activities_are_you_engaged_in_for_household_food_production_or_income_generation", "income_generation:") %>%
                                   str_replace("_are_","_") %>%
                                   str_remove("_remittances") %>%
                                   str_replace("_the_","_") %>%
                                   str_replace("receiving_assistance","financial assistance: amount") %>%
                                   str_replace("assistance_from_what_source", "financial assistance: source") %>%
                                   str_replace("assistance_usage", "financial assistance: usage") %>%
                                   str_remove("_for_heating-cooking")  %>%
                                   str_remove("_food_for_work") %>%
                                   str_remove("_agriculture_and_animal") %>%
                                   str_remove("you_had_") %>%
                                   str_remove("ngo") %>%
                                   str_remove("do_you_think_it_is_for_you_") %>%
                                   str_replace_all("_and_","&") %>%
                                   str_replace_all("_"," ") %>%
                                   str_to_title)) %>% 
        mutate(location = "Refugee Camp or Host Community",
               sd2 = "Relationship Status",
               sd4 = "Household: Responsible To Get Water",
               sd7 = "Household: Adult Members",
               sd8 = "Household: Elderly Members",
               sd9 = "Left Myanmar",
               sd13 = "House: Type",
               sd14 = "House: No. of Rooms",
               sd15 = "House: Has a Garden",
               sd16 = "House: Ownership",
               sd17 = "House: Electricity Supply",
               sd18 = "House: Piped Water Supply",
               sd19 = "House: Sewerage Connection",
               sd21 = "Rate Community: Socioeconomic Standing",
               sd22 = "Rate Community: Water Situation",
               w8 = "Experienced Problems & Solutions",
               p2 = "Women & Men: Equal Responsibility for Sanitation",
               p3 = "Women & Men: Equal Awareness of Feedback Processes",
               p4 = "Women & Men: Feedback Equally Valued",
               p5 = "Women & Men: Equal Awareness of Sanitation Rights",
               hw1 = "Worry About Water Supply",
               hw2 = "Supply Interruptions",
               hw2a = "Supply Interruptions: Expected or Unexpected",
               hw3 = "Unable to do Laundry Due to Water Situation",
               hw4 = "Schedule Change Due to Water Situation",
               hw5 = "Change What Was Eaten Due to Water Situation",
               hw6 = "Unable to Wash Hands After Dirty Activity",
               hw7 = "Unable to Wash Body Due to Water Situation",
               hw8 = "Not Enough Water to Drink",
               hw9 = "Felt Anger About Water Situation",
               hw10 = "Gone to Sleep Thirsty",
               hw11 = "No Useable or Drinkable Water",
               hw12 = "Felt Shame About Water Situation",
               hw13 = "Asked to Borrow Water",
               wat1 = "Drinking Water: Primary Source",
               wat2 = "Drinking Water: Secondary Source",
               wat3 = "Non-drinking Water: Primary Source",
               wat4 = "Drinking Water: Time to Source (mins)",
               wat5 = "Drinking Water: No. of Trips",
               wat9 = "Non-drinking Water: Time to Source (mins)",
               wat10 = "Non-drinking Water: No. of Trips",
               wat15_8 = "Fewest Water Problems: August",
               liv3 = "Income Generation: Primary Water Source",
               wb3 = "General Health",
               wb4 = "No. Days Poor Physical Health",
               wb5 = "No. Days Poor Mental Health",
               wb6 = "No. Days Health Prevented Normal Activities",
               wb7 = "Feel Unable to Control Important Things",
               wb8 = "Feel Confident in Ability to Control Problems",
               wb9 = "Feel Things Going Your Way",
               wb10 = "Feel Difficulties Could Not Be Overcome"
        )
      
      # Add newly created variables
      df <- df %>% mutate(
        sd5_ = "Household: Assists To Get Water",
        sd6_ = "Household: Children Under 18",
        wat12_ = "Drinking Water Treatment",
        wat12_6_ = "Drinking Water Treatment: Multiple Methods",
        wat15_ = "Fewest Water Problems",
        hw = "Calculated HWISE Score",
        liv1_ = "Income Generation",
        liv1_10_ = "Income Generation: Multiple Activities",
        liv4_ = "Income Generation: Problematic Water Quantity or Quality",
        wat5a = "Drinking Water: Collection Time Per Week (mins)",
        wat5b = "Drinking Water: Collection Time Per Week (grouped)"
      ) %>%
        relocate(sd5_, .after = sd4) %>%
        relocate(sd6_, .after = sd5_) %>%
        relocate(wat12_, .before = wat12_1) %>%
        relocate(wat12_6_, .after = wat12_5) %>%
        relocate(wat15_, .before = wat15_1) %>%
        relocate(hw, .before = hw1) %>%
        relocate(liv1_, .before = liv1_1) %>%
        relocate(liv1_10_, .after = liv1_9) %>%
        relocate(liv4_, .after = liv3) %>%
        relocate(wat5a, .after = wat5) %>%
        relocate(wat5b, .after = wat5a)
      
      # (WET) ----
    } else if (phase() == 2) {
      # Match cols to num.data()
      df <- df[, colnames(df) %in% colnames(num.data())]
      
      df <- df %>% mutate(across(everything(), 
                                 ~ str_replace(., "income_source_past_12_months_", "income_source:_") %>%
                                   str_replace("primary_way_household_treats_drinking_water","drinking_water_treatment:") %>%
                                   str_replace("your_household_experience_the_fewest_water_problems", "fewest_water_problems:") %>%
                                   str_replace("activities_are_you_engaged_in_for_household_food_production_or_income_generation", "income_generation:") %>%
                                   str_replace("_are_","_") %>%
                                   str_remove("_remittances") %>%
                                   str_replace("_the_","_") %>%
                                   str_replace("receiving_assistance","financial assistance: amount") %>%
                                   str_replace("assistance_from_what_source", "financial assistance: source") %>%
                                   str_replace("assistance_usage", "financial assistance: usage") %>%
                                   str_remove("_for_heating-cooking")  %>%
                                   str_remove("_food_for_work") %>%
                                   str_remove("_agriculture_and_animal") %>%
                                   str_remove("you_had_") %>%
                                   str_remove("ngo") %>%
                                   str_remove("do_you_think_it_is_for_you_") %>%
                                   str_replace_all("_and_","&") %>%
                                   str_replace_all("_"," ") %>%
                                   str_to_title)) %>% 
        mutate(location = "Refugee Camp or Host Community",
               sd2 = "Relationship Status",
               sd4 = "Household: Responsible To Get Water",
               sd7 = "Household: Adult Members",
               sd8 = "Household: Elderly Members",
               sd12 = "House: Type",
               sd13 = "House: No. of Rooms",
               sd14 = "House: Has a Garden",
               sd15 = "House: Ownership",
               sd16 = "House: Electricity Supply",
               sd17 = "House: Piped Water Supply",
               sd18 = "House: Sewerage Connection",
               sd20 = "Rate Community: Socioeconomic Standing",
               sd21 = "Rate Community: Water Situation",
               li7 = "Income: Difficult to Obtain",
               li8 = "Income: Problems Experienced",
               li8_1 = "Income: How Compensate for Problems",
               wb2 = "Feel Unable to Control Important Things",
               wb4 = "General Health",
               wb5 = "No. Days Poor Physical Health",
               wb6 = "No. Days Poor Mental Health",
               wb9a = "Injured While Fetching Water: How",
               wb9b = "Injured While Fetching Water: Where",
               wb9c = "Injured While Fetching Water: Injury",
               wat1 = "Drinking Water: Primary Source",
               wat2 = "Drinking Water: Secondary Source",
               wat3 = "Drinking Water: Time to Source (mins)",
               wat4 = "Drinking Water: No. of Trips",
               wat5 = "Drinking Water: Queue Time",
               wat6 = "Drinking Water: Litres Each Day",
               wat10 = "Non-drinking Water: Primary Source",
               wat11 = "Non-drinking Water: Time to Source (mins)",
               wat12 = "Non-drinking Water: No. of Trips",
               wat13 = "Non-drinking Water: Queue Time",
               wat14 = "Non-drinking Water: Litres Each Day",
               hw1 = "Worry About Water Supply",
               hw2 = "Supply Interruptions",
               hw2a = "Supply Interruptions: Expected or Unexpected",
               hw3 = "Unable to do Laundry Due to Water Situation",
               hw4 = "Schedule Change Due to Water Situation",
               hw5 = "Change What Was Eaten Due to Water Situation",
               hw6 = "Unable to Wash Hands After Dirty Activity",
               hw7 = "Unable to Wash Body Due to Water Situation",
               hw8 = "Not Enough Water to Drink",
               hw9 = "Felt Anger About Water Situation",
               hw10 = "Gone to Sleep Thirsty",
               hw11 = "No Useable or Drinkable Water",
               hw12 = "Felt Shame About Water Situation",
               hw13 = "Asked to Borrow Water",
               pay4_1 = "Who Do You Pay Fee To: Specify",
               pay5_1 = "How Do You Pay Fees: Specify"
        )
      
      df[c("wat19","wat20","wat21","wat22","wat23","wat24","wat25")] <- lapply(df[c("wat19","wat20","wat21","wat22","wat23","wat24")], function(x) paste("Water Storage: ", x, sep = " "))
      
      # Add newly created variables
      df <- df %>% mutate(
        sd5_ = "Household: Assists To Get Water",
        sd6_ = "Household: Children Under 18",
        wat9_ = "Drinking Water Treatment",
        hw = "Calculated HWISE Score",
        wat5a = "Drinking Water: Collection Time Per Week (mins)",
        wat5b = "Drinking Water: Collection Time Per Week (grouped)",
        pay2a = "How Much Do You Pay (grouped)"
      ) %>%
        relocate(sd5_, .after = sd4) %>%
        relocate(sd6_, .after = sd5_) %>%
        relocate(hw, .before = hw1) %>%
        relocate(wat5a, .after = wat5) %>%
        relocate(wat5b, .after = wat5a) %>%
        relocate(wat9_, .after = wat8) %>%
        relocate(pay2a, .after = pay2)
      
      return(df)
    }
  })
  
  ### Text Data ----
  # A cleaned and tidied df of all text responses
  # Accurate variable classes
  
  text.data <-  reactive({
    data_file <- input$file
    # Read Excel
    df <- try(xl.read.file(data_file$datapath,
                           header = FALSE,
                           password = input$pword,
                           top.left.cell = "A3")) # ignores row 1 & 2 (chr), ensures single accurate class per col
    
    # df <- try(xl.read.file("~/DataScienceMsc/dissertation/HWISE Visualiser/data/Socio-Economic Survey, Teknaf Ground Water Phase 2_LABELS - Copy.csv",
    #                        header = FALSE,
    #                        top.left.cell = "A3"))
    
    # (DRY) ----
    if (phase() == 1) {
      # Set colnames as question codes
      colnames(df) <- colnames(raw.data())
      
      # match cols to data in num.data
      df <- df[, colnames(df) %in% colnames(num.data())]
      
      # Add newly created variables
      `%!in%` <- Negate(`%in%`)
      df <- bind_cols(df, num.data()[, colnames(num.data()) %!in% colnames(df)])
      df <- df %>%
        relocate(sd5_, .after = sd4) %>%
        relocate(sd6_, .after = sd5_) %>%
        relocate(wat12_, .before = wat12_1) %>%
        relocate(wat12_6_, .after = wat12_5) %>%
        relocate(wat15_, .before = wat15_1) %>%
        relocate(hw, .before = hw1) %>%
        relocate(liv1_, .before = liv1_1) %>%
        relocate(liv1_10_, .after = liv1_9) %>%
        relocate(liv4_, .after = liv3) %>%
        relocate(wat5a, .after = wat5) %>%
        relocate(wat5b, .after = wat5a)
      
      # Insert correct text responses
      df$camp <- num.data()$camp
      
      df$sd11_2 <- num.data()$sd11_2
      
      df$ward <- paste("Ward", df$ward, sep = " ")
      
      df$sd5_ <- sapply(df$sd5_, function(x) {
        switch(x, "Self", "Spouse", "Children: Boy", "Children: Girl", "Other", "Shared Responsibility")})
      
      df$sd9 <- num.data()$sd9
      
      ## Define a list of all binary questions to format together
      binary_q <- c("sd11","sd15","sd17","sd18","sd19","w1_1","w2","w3","w4","w5",
                    "w6","w7","wat6","wat7","wat11","wat12_1","wat12_2","wat12_3","wat12_4",
                    "wat12_5","wat12_6_","wat15_1","wat15_2","wat15_3","wat15_4","wat15_5","wat15_6",
                    "wat15_7","wat15_8","wat15_9","wat15_10","wat15_11","wat15_12","liv1_1","liv1_2",
                    "liv1_3","liv1_4","liv1_5","liv1_6","liv1_7","liv1_8","liv1_9","liv1_10_","liv2")
      ## format all binaries
      df[binary_q] <- lapply(df[binary_q], function(x){
        x = replace(x, which(x==0),"No")
        x = replace(x, which(x==1),"Yes")
        x = replace(x, which(x==2),"Don't Know")
        x = replace(x, which(x==88),"NA")})
      
      df$sd20 <- sapply(df$sd20, function(x) {
        x = replace(x, which(x==1),"Wood")
        x = replace(x, which(x==2),"Gas Bottles")
        x = replace(x, which(x==3),"Other")}) 
      
      df[c("wat1","wat2","wat3","liv3")] <- lapply(df[c("wat1","wat2","wat3","liv3")], function(x){
        x = replace(x, which(x==1),"Piped Supply")
        x = replace(x, which(x==2),"Stand Pipe")
        x = replace(x, which(x==3),"Borehole/Tubewell")
        x = replace(x, which(x==4),"Dug Well: Protected")
        x = replace(x, which(x==5),"Dug Well: Unrotected")
        x = replace(x, which(x==6),"Spring: Protected")
        x = replace(x, which(x==7),"Spring: Unprotected")
        x = replace(x, which(x==8),"Rainwater Collection")
        x = replace(x, which(x==9),"Small Water Vendor")
        x = replace(x, which(x==10),"Tanker Truck")
        x = replace(x, which(x==11),"Bottled Water")
        x = replace(x, which(x==12),"Sachet Water")
        x = replace(x, which(x==13),"Surface Water/Pond/River/Lake")
        x = replace(x, which(x==14),"Other Person")
        x = replace(x, which(x==15),"Other")
        x = replace(x, which(is.na(x)),"NA")
      })
      # # Change to grouped values 
      df$wat5b <- sapply(df$wat5b, function(x){
        x = replace(x, which(x==1), "0-100 mins")
        x = replace(x, which(x==2), "101-200 mins")
        x = replace(x, which(x==3), "201-300 mins")
        x = replace(x, which(x==4), "301-400 mins")
        x = replace(x, which(x==5), "401-500 mins")
        x = replace(x, which(x==6), "501-600 mins")
        x = replace(x, which(x==7), "600+ mins")
      })
      
      df$wat12_ <- sapply(df$wat12_, function(x){
        switch(x+1, "None","Boil","Filter","Add Chemicals/Chlorine","Other","Combined Methods")})
      
      df$wat15_ <- sapply(df$wat15_, function(x) {
        switch(x, "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Multiple Months")})
      
      df$liv1_ <- sapply(df$liv1_, function(x) {
        switch(x+1, "None of the Above", "Crop Production","Betel Nut/Leaf Production",
               "Livestock Production","Salt Production","Sea Fishing","Shrimp Farming",
               "Fish Farming","Other","Multiple Activities")})
      
      df$liv4_ <- sapply(df$liv4_, function(x){
        switch(x+1, "No","Quantity","Quality","Both Quantity & Quality")})
      
      df[, grepl("^hw.*\\d$", names(df))] <- apply(df[, grepl("^hw.*\\d$", names(df))], 2, function(x){
        x = replace(x, which(x=="0_times"), "Never")
        x = replace(x, which(x=="1-2_times"), "Rarely")
        x = replace(x, which(x=="3-10_times"), "Sometimes")
        x = replace(x, which(x=="11-20_times"), "Often")
        x = replace(x, which(x=="more_than_20_times"), "Always")
        x = replace(x, which(x=="dont_know"), "Don't Know")
      })
      
      df[, grepl("pay", names(df))] <- apply(df[,grepl("pay", names(df))], 2, function(x){
        x = replace(x, which(x=="Fixed_cost_per_amount"), "Fixed: Per Amount")
        x = replace(x, which(x=="Fixed_cost_per_month"), "Fixed: Per Month")
        x = replace(x, which(x=="VC_permonth_on_set_factors"), "Variable: Set Factors")
        x = replace(x, which(x=="Variable_cost_per_amount_or_use"), "Variable: Per Amount")
        x = replace(x, which(x=="no_fixed_payment_is_made"), "No Fixed Payment")
        x = replace(x, which(x=="it_is_free"), "Free")
      })    
      
      df <-  df %>% mutate(across(where(is.character),
                                  ~ str_replace_all(., "_and_", "_&_") %>%
                                    str_replace_all("_", " ") %>%
                                    str_to_title))
      
      df <- df %>% mutate(across(c("w8","wat8","wat16","wat17","liv6","liv5"), 
                                 ~ str_replace_all(., "_and_", "_&_") %>%
                                   str_replace_all("_", " ") %>%
                                   str_replace_all("\n", " ") %>%
                                   str_squish %>%
                                   str_to_sentence))
      
      df[is.na(df)] <- "NA"
      
      return(df)
      
      # (WET) ----
    } else if (phase() == 2) {
      # Set colnames as question codes
      colnames(df) <- colnames(raw.data())
      
      # match cols to data in num.data
      df <- df[, colnames(df) %in% colnames(num.data())]
      
      # Add newly created variables
      `%!in%` <- Negate(`%in%`)
      df <- bind_cols(df, num.data()[, colnames(num.data()) %!in% colnames(df)])
      df <- df %>%
        relocate(sd5_, .after = sd4) %>%
        relocate(sd6_, .after = sd5_) %>%
        relocate(hw, .before = hw1) %>%
        relocate(wat5a, .after = wat5) %>%
        relocate(wat5b, .after = wat5a) %>%
        relocate(wat9_, .after = wat8) %>%
        relocate(pay2a, .after = pay2)
      
      # Insert correct text responses
      df$camp <- num.data()$camp
      
      df$pay9 <- num.data()$pay9
      
      df$ward <- paste("Ward", df$ward, sep = "_")
      
      df$sd5_ <- sapply(df$sd5_, function(x) {
        switch(x, "Self", "Spouse", "Children: Boy", "Children: Girl", "Other", "Shared Responsibility")})
      
      ## Define a list of all binary questions to format together
      binary_q <- c("sd9","sd16","sd17","sd14","sd18",
                    "li1","li2","li3","li4","li5","li6","li7",
                    "wb7","wb8","wat7","pay1","pay6")
      ## format all binaries
      df[binary_q] <- lapply(df[binary_q], function(x){
        x = replace(x, which(x==0),"No")
        x = replace(x, which(x==1),"Yes")
        })
      
      df[,] <- lapply(df[,], function(x){
        x = replace(x, which(x==77),"Other")
        x = replace(x, which(x==88),"Don't Know")
      })
      
      # wat
      df[c("wat1","wat2","wat10")] <- lapply(df[c("wat1","wat2","wat10")], function(x){
        x = replace(x, which(x==1),"Piped Supply")
        x = replace(x, which(x==2),"Stand Pipe")
        x = replace(x, which(x==3),"Borehole/Tubewell")
        x = replace(x, which(x==4),"Dug Well: Protected")
        x = replace(x, which(x==5),"Dug Well: Unrotected")
        x = replace(x, which(x==6),"Spring: Protected")
        x = replace(x, which(x==7),"Spring: Unprotected")
        x = replace(x, which(x==8),"Rainwater Collection")
        x = replace(x, which(x==9),"Small Water Vendor")
        x = replace(x, which(x==10),"Tanker Truck")
        x = replace(x, which(x==11),"Bottled Water")
        x = replace(x, which(x==12),"Sachet Water")
        x = replace(x, which(x==13),"Surface Water/Pond/River/Lake")
        x = replace(x, which(x==14),"Other Person")
        x = replace(x, which(x==77),"Other")
        x = replace(x, which(is.na(x)),"NA")
      })
      
      df$wat5b <- sapply(df$wat5b, function(x){
        x = replace(x, which(x==1), "0-100 mins")
        x = replace(x, which(x==2), "101-200 mins")
        x = replace(x, which(x==3), "201-300 mins")
        x = replace(x, which(x==4), "301-400 mins")
        x = replace(x, which(x==5), "401-500 mins")
        x = replace(x, which(x==6), "501-600 mins")
        x = replace(x, which(x==7), "600+ mins")
      })
      
      df$wat9_ <- sapply(df$wat9_, function(x){
        x = replace(x, which(x==0), "None")
        x = replace(x, which(x==1), "Boil")
        x = replace(x, which(x==2), "Filter")
        x = replace(x, which(x==3), "Add Chemicals/Chlorine")
        x = replace(x, which(x==4), "Combined Methods")
        x = replace(x, which(x==77), "Other")
      })
      
      df$wat27 <- substr(df$wat27,1,3)
      
      df$pay2a <- sapply(df$pay2a, function(x){
        x = replace(x, which(x==0), "0-100")
        x = replace(x, which(x==1), "101-200")
        x = replace(x, which(x==2), "201-300")
        x = replace(x, which(x==3), "301-400")
        x = replace(x, which(x==4), "401-500")
        x = replace(x, which(x==5), "501-600")
        x = replace(x, which(x==6), "601-700")
        x = replace(x, which(x==7), "701-800")
        x = replace(x, which(x==8), "801-900")
        x = replace(x, which(x==9), "900+")
      })
      
      df$pay3 <- sapply(df$pay3, function(x){
        x = replace(x, which(x=="Fixed_cost_per_amount_"), "Fixed: Per Amount")
        x = replace(x, which(x=="Fixed_cost_per_month"), "Fixed: Per Month")
        x = replace(x, which(x=="VC_permonth_on_set_factors"), "Variable: Set Factors")
        x = replace(x, which(x=="Variable_cost_per_amount_or_use"), "Variable: Per Amount")
        x = replace(x, which(x=="No_fixed_payment_is_made"), "No Fixed Payment")
        x = replace(x, which(x=="it_is_free"), "Free")
      }) 
      
      # HWISE qs
      df[, grepl("^hw.*\\d$", names(df))] <- apply(df[, grepl("^hw.*\\d$", names(df))], 2, function(x){
        x = replace(x, which(x=="0_times"), "Never")
        x = replace(x, which(x=="1-2_times"), "Rarely")
        x = replace(x, which(x=="3-10_times"), "Sometimes")
        x = replace(x, which(x=="11-20_times"), "Often")
        x = replace(x, which(x=="more_than_20_times"), "Always")
        x = replace(x, which(x=="dont_know"), "Don't Know")
      })
      
      ## String manipulation
      df <-  df %>% mutate(across(where(is.character),
                                  ~ str_replace_all(., "_and_", "_&_") %>%
                                    str_replace_all("_", " ") %>%
                                    str_replace_all("Myself", "Self") %>%
                                    str_replace_all("My Spouse", "Spouse") %>%
                                    str_replace_all("Dont", "Don't") %>%
                                    str_replace_all("Taknaf", "Teknaf") %>%
                                    str_replace_all("Almost", "Most") %>%
                                    str_to_title%>%
                                    str_replace_all("Nrc", "NRC") %>%
                                    str_replace_all("Na","NA")))
      
      freetxt <- c("li8","li8_1",
                   "wb9a","wb9b","wb9c",
                   "wat2a","wat9_5","wat8","wat28a","wat28b","wat29",
                   "pay4_1","pay5_1")
      df[,freetxt] <- df[,freetxt] %>% mutate(across(everything(),
                                 str_to_sentence))
      
      # Define numeric variables
      df <- df %>%
        mutate(across(c("gps-latitude","gps-longitude","sd3","sd6_","sd7","sd8","sd10","sd13","sd20","sd21","wb5","wb6",
                        "wat3","wat4","wat5","wat5a","wat6","wat11","wat12","wat13","wat14",
                        "wat19","wat20","wat21","wat22","wat23","wat24","hw",
                        "pay2","pay9"), as.numeric))
      
      df$pay2[is.na(df$pay2)] <- 0
      df[is.na(df)] <- "NA"
      
      return(df)
    }
    
  })
  
  ### Filtered clean data ---- 
  # Create a data object so that data can be filtered for display
  # using grouped UI options
  filter_clean <- reactive({
    
      # (DRY) ----
      if (phase() == 1) {
        varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(num.data()[, grepl("sd", names(num.data()))])),
        "Work"=c("w1_1","w2","w3","w4","w5","w6","w7","w8"),
        "Perception of Water Services"=c("p1","p2","p3","p4","p5"),
        "HWISE"=c(colnames(num.data()[, grepl("hw", names(num.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(num.data()[, grepl("wat", names(num.data()))])),
        "Livelihood Activities"=c(colnames(num.data()[, grepl("liv", names(num.data()))])),
        "Paying for Water"=c(colnames(num.data()[, grepl("pay", names(num.data()))])),
        "Well-being"=c(colnames(num.data()[, grepl("wb", names(num.data()))]))
      )
      
      # (WET) ----
      } else if (phase() ==2) {
        varGroups <- list(
          "Location" = "location",
          "Union" = "union",
          "Ward" = "ward",
          "Village" = "village",
          "Camp" = "camp",
          "Block" = "block",
          "Sex"="sex",
          "Sociodemographic"=c(colnames(num.data()[, grepl("sd", names(num.data()))])),
          "Livelihood & Income"=c(colnames(num.data()[, grepl("li", names(num.data()))])),
          "HWISE"=c(colnames(num.data()[, grepl("hw", names(num.data()))])),
          "Water Access, Quantity, Utility, Stability"=c(colnames(num.data()[, grepl("wat", names(num.data()))])),
          "Paying for Water"=c(colnames(num.data()[, grepl("pay", names(num.data()))])),
          "Well-being"=c(colnames(num.data()[, grepl("wb", names(num.data()))]))
        )
      }
    
    names <- unlist(lapply(input$varchoices, function(x) varGroups[[x]]))
    num.data() %>% select(all_of(names))
  })
  
  ### Filtered tidy raw data ----
  # Create a data object so that data can be filtered for display
  # using grouped UI options
  filter_tidy <- reactive({
    # (DRY) ----
    if (phase() == 1) {
      varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Work"=c("w1_1","w2","w3","w4","w5","w6","w7","w8"),
        "Perception of Water Services"=c("p1","p2","p3","p4","p5"),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Livelihood Activities"=c(colnames(text.data()[, grepl("liv", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    # (WET) ----
    } else if (phase() ==2) {
      varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Livelihood & Income"=c(colnames(text.data()[, grepl("li", names(text.data()))])),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    }
    
    names <- unlist(lapply(input$varchoices1, function(x) varGroups[[x]]))
    text.data() %>% select(all_of(names))
  })
  
  ### Filtered questions table ----
  # * for legend within sidebar
  filter_Q <- reactive({
    df <- data.frame(Question = unlist(questions_vec()))
    df %>% filter(rownames(df) %in% colnames(filter_clean()))
  })
  
  filter_Q1 <- reactive({
    df <- data.frame(Question = unlist(questions_vec()))
    df %>% filter(rownames(df) %in% colnames(filter_tidy()))
  })
  
  # Render filtered Q clean data
  output$filter_Q <- renderDataTable(
    filter_Q(), rownames = TRUE,
    class = "compact",
    options = list(searching = FALSE,
                   pageLength = 25,
                   paging = FALSE)
  )
  
  # Render filtered Q tidy raw data
  output$filter_Q1 <- renderDataTable(
    filter_Q1(), rownames = TRUE,
    class = "compact",
    options = list(searching = FALSE,
                   pageLength = 25,
                   paging = FALSE)
  )

  
  ## Render data objects ----
  
  # Render raw.data output
  output$raw_data <- renderDataTable({
    raw.data()
  })
  
  output$raw_dataA <- renderDataTable({
    text.data()
  })
  
  output$num.data <- renderDataTable({
    num.data()
  })
  
  # Render filtered clean
  output$filter_clean <- renderDataTable(
    filter_clean(), rownames=TRUE,
    options = list(pageLength = 25)
  )
  
  # Render filtered tidy raw
  output$filter_tidy <- renderDataTable(
    filter_tidy(), rownames = TRUE,
    options = list(pageLength = 25)
  )
  
  # Summary tab ------------------------------------------------------------------
  
  ## UI elements ----------
  
  # Group select
  output$group_stats <- renderUI({
    pickerInput("group_stats", label = "Choose variable to group statistics by", 
                choices = c("None", c("location","union","ward","village","camp","sex")),
                selected = "None",
                options = list(`style` = "btn-primary"))
  })
  
  # Table select 
  output$choose_tables <- renderUI({
    radioButtons("choose_tables", "Select variables for summary:",
                 choices  = list("Categorical", "Numeric"),
                 selected = "Numeric"
    )
  })
  
  # Variables toggle
  output$var_toggle1 <- renderUI({
    if (phase() == 1) {
      radioButtons(
        "var_toggle1", "Select variables to display:",
        choices = c("Location", "Sex", "Camp", "Union", "Ward", "Village","Sociodemographic","Work","Perception of Water Services",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Livelihood Activities","Paying for Water","Well-being"),
        selected = "Location")
    } else if (phase() == 2) {
      radioButtons(
        "var_toggle1", "Select variables to display:",
        choices = c("Location", "Sex", "Camp", "Union", "Ward", "Village",
                    "Sociodemographic","Livelihood & Income",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Paying for Water","Well-being"),
        selected = "Location")
    }
  })
  
  output$var_toggle <- renderUI({
    if (phase() == 1) {
      radioButtons(
        "var_toggle", "Select variables to display:",
        choices = c("Sociodemographic","Work","Perception of Water Services",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Livelihood Activities","Paying for Water","Well-being"),
        selected = "Sociodemographic")
    } else if (phase() == 2) {
      radioButtons(
        "var_toggle", "Select variables to display:",
        choices = c("Location", "Sex", "Camp", "Union", "Ward", "Village",
                    "Sociodemographic","Livelihood & Income",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Paying for Water","Well-being"),
        selected = "Sociodemographic")
    }
  })
  
  ## Observe events ----
  observe({
    toggle(id="fac_table", condition = "Categorical" %in% input$choose_tables)
    toggle(id="num_table", condition = "Numeric" %in% input$choose_tables)
  })
  
  ## Summary data ----------
  
  # Define custom skim (display full names for factor levels)
  my_skim <- skim_with(factor = sfl(top_counts = ~top_counts(., max_char = 25, max_levels = 50)),
                       numeric = list(hist = NULL))
  
  summary.data <- reactive({
    group_val <- input$group_stats
    if(input$group_stats == "None") {group_val <- NULL}
    
    # (DRY) ----
    if (phase() == 1 ) {
      df <- text.data() %>%
        mutate(across(which(sapply(.,class)!="numeric"),factor)) %>%
        select(-c("gps-longitude","gps-latitude",sd11_3,w8,wat8,wat16,wat17,liv5,liv6)) %>%
        group_by_at(group_val) %>%
        my_skim %>%
        focus(c(
          skim_variable,
          all_of(group_val),
          n_missing,
          numeric.mean,
          numeric.p0,
          numeric.p50,
          numeric.p100,
          numeric.sd,
          factor.n_unique,
          factor.top_counts))
    
    # (WET) ----
    } else if (phase() == 2) {
      df <- text.data() %>%
        mutate(across(which(sapply(.,class)!="numeric"),factor)) %>%
        select(-c("gps-longitude","gps-latitude",li8,li8_1,wat8,wat28a,wat28b,wat29,
                  wb9a,wb9b,wb9c)) %>%
        group_by_at(group_val) %>%
        my_skim %>%
        focus(c(
          skim_variable,
          all_of(group_val),
          n_missing,
          numeric.mean,
          numeric.p0,
          numeric.p50,
          numeric.p100,
          numeric.sd,
          factor.n_unique,
          factor.top_counts))
    }
    
    df <- data.frame(df) %>%
      rename("Type" = "skim_type",
             "Variable" = "skim_variable",
             "Missing" = "n_missing",
             "Mean" = "numeric.mean",
             "Min" = "numeric.p0",
             "Median" = "numeric.p50",
             "Max" = "numeric.p100",
             "Standard Deviation" = "numeric.sd",
             "Category Levels" = "factor.n_unique",
             "Category Level Counts" = "factor.top_counts") 
    df <- df %>% mutate_if(is.numeric, round, 1)
    return(df)
  })
  
  ### Render data tables ----
  output$sum_factor <- renderDataTable({
    if (phase() == 1) {
      varGroups <- list(
        "Location"="location", "Sex"="sex","Camp"="camp","Union"="union","Ward"="ward","Village"="village",
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Work"=c("w1_1","w2","w3","w4","w5","w6","w7","w8"),
        "Perception of Water Services"=c("p1","p2","p3","p4","p5"),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Livelihood Activities"=c(colnames(text.data()[, grepl("liv", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    } else if(phase() == 2) {
      varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Livelihood & Income"=c(colnames(text.data()[, grepl("li", names(text.data()))])),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    }
    
    names <- unlist(lapply(input$var_toggle1, function(x) varGroups[[x]]))
    
    df <- summary.data()%>%
      filter(Type == "factor") %>%
      select(-c(`Type`,`Mean`,`Min`,`Median`, `Max`, `Standard Deviation`))%>%
      mutate(Question = sapply(Variable, function(x) questions_vec()[[x]])) %>%
      relocate(Question, .after = Variable)
    
    datatable(df[df$Variable %in% names, ],
              rownames = FALSE,
              caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:#3aa9e8; font-size:200% ;','Categorical Variables')
    )
    
  })
  
  output$sum_num <- renderDataTable({
    if (phase() == 1) {
      varGroups <- list(
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Work"=c("w1_1","w2","w3","w4","w5","w6","w7","w8"),
        "Perception of Water Services"=c("p1","p2","p3","p4","p5"),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Livelihood Activities"=c(colnames(text.data()[, grepl("liv", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    } else if (phase() ==2) {
      varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Livelihood & Income"=c(colnames(text.data()[, grepl("li", names(text.data()))])),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    }
 
    
    names <- unlist(lapply(input$var_toggle, function(x) varGroups[[x]]))
    
    df <- summary.data()%>%
      filter(Type == "numeric") %>%
      select(-c(`Category Levels`,`Category Level Counts`, `Type`))%>%
      mutate(Question = sapply(Variable, function(x) questions_vec()[[x]])) %>%
      relocate(Question, .after = Variable)
    
    datatable(df[df$Variable %in% names, ],
              rownames = FALSE,
              caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:#3aa9e8; font-size:200% ;','Numeric Variables')
    )
    
  })
  output$sum_plot <- renderPlot({
    req(input$group_stats)
    x <- num.data()[[input$group_stats]]
    temp <- data.frame(cbind(x))
    
    paletteFunc <- colorRampPalette(c("#18db50","#1d6cac","#00b4ff"))
    
    p1<- ggplot(na.omit(temp),aes(factor(x))) +
      geom_bar(fill = paletteFunc(length(levels(factor(x))))) +
      labs(title = paste0(questions_vec()[[input$group_stats]]),
           x = NULL) +
      scale_x_discrete(breaks = x,
                       labels = text.data()[[input$group_stats]]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    if(input$group_stats!="None") {return(p1)}
  })
  
  # Plots tab --------------------------------------------------------------------
  
  ## Set variable groups  ----------
  
  # factor
  factor_vars <- reactive({
    df <- questions_vec() %>% select(
      c("location","sex","camp","union","ward")
    )
    return(df)
  })
  
  # categorical
  cat_vars <- reactive({
    if (phase() ==1) {
      df <- questions_vec() %>% select(
        "location","sex","camp","union","ward",
        starts_with("sd"), -c("sd3","sd6_","sd7","sd8","sd11_1","sd11_3","sd14","sd21","sd22"),
        c("w1_1","w2","w3","w4","w5","w5","w6","w7"),
        c("p1","p2","p3","p4","p5"),
        starts_with("hw"), -"hw",
        starts_with("wat"), -c("wat4","wat5","wat5a","wat8","wat9","wat10","wat14","wat14_1","wat14_2","wat14_3","wat16","wat17"),
        starts_with("liv"), -c("liv5","liv6"),
        starts_with("pay"),
        starts_with("wb"), -c("wb4","wb5","wb6")
      )
      return(df)
      
    } else if (phase() == 2) {
      df <- questions_vec() %>% select(
        "location","sex","camp","union","ward",
        starts_with("sd"), -c("sd3","sd6_","sd7","sd8","sd10","sd10_1","sd10_2",
                              "sd12","sd13","sd14","sd20","sd21"),
        starts_with("li"), -c("li8","li8_1"),
        starts_with("wb"), -c("wb3","wb5","wat5a","wb6","wb9a","wb9b","wb9c"),
        starts_with("wat"), -c("wat2a","wat3","wat4","wat5","wat6","wat8","wat9_5",
                               "wat11","wat12","wat13","wat14","wat19","wat20",
                               "wat21","wat22","wat23","wat24","wat25",
                               "wat28a","wat28b","wat29"),
        starts_with("hw"), -"hw",
        starts_with("pay"), -c("pay2","pay4","pay4_1","pay5_1","pay9")
      )
    }
  })
  
  # continuous
  cont_vars <- reactive({
    if(phase() ==1) {
      df <- questions_vec() %>% select(
        c("sd3","sd6_","sd7","sd8","sd11_1","sd14","sd21","sd22"),
        starts_with("hw"), -"hw2a",
        c("wat4","wat5","wat5a","wat9","wat10","wat14","wat14_1","wat14_2"),
        starts_with("wb"), -"wb2",
        c("pay1","pay2")
      )
      return(df)
    } else if (phase() == 2){
      df <- questions_vec() %>% select(
        c("sd3","sd6_","sd7","sd8","sd10","sd13","sd20","sd21"),"li9",
        starts_with("wb"), -c("wb3","wb7","wb8","wb9a","wb9b","wb9c"),
        starts_with("wat"), -c("wat1","wat2","wat2a","wat7","wat8","wat9_","wat9_5","wat10","wat28a","wat28b","wat29"),
        starts_with("hw"), -"hw2a",
        c("pay2","pay9")
      )
    }
  })
  
  ## UI elements ----------
  
  # Bar Plot
  ## x select
  output$variable_x_bar <- renderUI({
    
    pickerInput("variable_x_bar", label = "Set horizontal axis", 
                choices = c(colnames(cat_vars())),
                choicesOpt = list(
                  subtext = paste(cat_vars()[1, ])
                ),
                options = list(
                  `live-search` = TRUE)
    ) 
  })
  ## group select
  output$variable_groupby_bar <- renderUI({
    pickerInput("variable_groupby_bar", label = "Choose variable to group by", 
                choices = c(names(factor_vars()),"None"),
                selected = "None",
                options = list(
                  `live-search` = TRUE)
    )
  })
  
  # Box Plot
  ## y select
  output$variable_y_box <- renderUI({
    pickerInput("variable_y_box", label = ("Set vertical axis:"),
                choices = c(colnames(cont_vars())),
                selected = "hw",
                choicesOpt = list(
                  subtext = paste(cont_vars()[1, ])
                ),
                options = list(
                  `live-search` = TRUE)
    )  
  })
  
  ## x select
  output$variable_x_box <- renderUI({
    pickerInput("variable_x_box", label = "Set horizontal axis", 
                choices = c(colnames(cat_vars())),
                selected = "hw1",
                choicesOpt = list(
                  subtext = paste(cat_vars()[1, ])
                ),
                options = list(
                  `live-search` = TRUE)
    ) 
  })
  
  ## group select
  output$variable_groupby_box <- renderUI({
    pickerInput("variable_groupby_box", label = "Choose variable to group by", 
                choices = c(names(factor_vars()),"None"),
                selected = "None",
                options = list(
                  `live-search` = TRUE)
    )
  })
  
  # Scatter Plot
  ## y select
  output$variable_y_scat <- renderUI({
    pickerInput("variable_y_scat", label = ("Set vertical axis:"),
                choices = c(colnames(cont_vars())),
                selected = "hw",
                choicesOpt = list(
                  subtext = paste(cont_vars()[1, ])
                ),
                options = list(
                  `live-search` = TRUE)
    )  
  })
  
  ## x select
  output$variable_x_scat <- renderUI({
    pickerInput("variable_x_scat", label = "Set horizontal axis", 
                choices = c(colnames(cont_vars())),
                selected = "hw1",
                choicesOpt = list(
                  subtext = paste(cont_vars()[1, ])
                ),
                options = list(
                  `live-search` = TRUE)
    ) 
  })
  
  ## group select
  output$variable_groupby_scat <- renderUI({
    pickerInput("variable_groupby_scat", label = "Choose variable to group by", 
                choices = c(names(factor_vars()),"None"),
                selected = "None",
                options = list(
                  `live-search` = TRUE)
    )
  })
  
  ## Plots ----------
  
  ### Set theme ----
  my_theme <- theme(plot.title = element_text(size=18, color="#464646",face="bold", hjust=0.5),
                    axis.title = element_text(size=15, color="#464646"),
                    axis.text = element_text(size=12, color="#464646"),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                    legend.title = element_text(size=15, color="#464646",face="bold",hjust=0.5),
                    legend.text = element_text(size=12, color="#464646"),
                    legend.background = element_rect(fill = "#f2f2f2"),
                    panel.background = element_rect(fill = "#f2f2f2"),
                    plot.margin = margin(1,1,1,1, "cm"))
  
  ### Bar plots ----------
  
  output$bar_plot <- renderPlotly({
    req(input$variable_groupby_bar)
    x <- num.data()[[input$variable_x_bar]]
    g <- text.data()[[input$variable_groupby_bar]]
    
    labelsx = text.data()[[input$variable_x_bar]]
    temp <- data.frame(x, stringsAsFactors = FALSE)
    
    paletteFunc <- colorRampPalette(c("#18db50","#1d6cac","#00b4ff"))
    
    if(input$variable_groupby_bar != "None") {
      
      p1 <- # grouped
        temp %>% 
        ggplot +
        aes(factor(x), fill = g) +
        geom_bar() +
        scale_fill_manual(values = paletteFunc(length(levels(factor(g))))) +
        scale_x_discrete(breaks = x, labels = labelsx)  +
        labs(title = paste0(questions_vec()[[input$variable_x_bar]]),
             x = NULL,
             fill = paste0(questions_vec()[[input$variable_groupby_bar]])) +
        my_theme
      
      ggplotly(p1, tooltip = c("y","fill"))
    } else{
      
      
      p2 <- # not grouped
        temp %>% 
        ggplot +
        aes(factor(x)) +
        geom_bar(fill = paletteFunc(length(unique(x)))) +
        scale_x_discrete(breaks = x,
                         labels = labelsx) +
        labs(title = paste0(questions_vec()[[input$variable_x_bar]]),
             x = NULL) +
        my_theme
      
      ggplotly(p2, tooltip = "y")
    }
  })
  
  ### Box plots ----------
  
  output$box_plot <- renderPlotly({
    req(input$variable_groupby_box)
    paletteFunc <- colorRampPalette(c("#18db50","#1d6cac","#00b4ff"))
    plot_cols <- paletteFunc(length(levels(factor(num.data()[[input$variable_groupby_box]]))))
    
    x <- num.data()[[input$variable_x_box]]
    y <- num.data()[[input$variable_y_box]]
    g <- text.data()[[input$variable_groupby_box]]
    labelsx = text.data()[[input$variable_x_box]]
    labelsy = text.data()[[input$variable_y_box]]
    temp <- data.frame(x, y, stringsAsFactors = FALSE)
    
    # grouped
    base_plot_p1 <- temp %>% 
      ggplot +
      aes(factor(x), y, fill = g)+
      geom_boxplot(varwidth = TRUE,
                   alpha = 0.8,
                   outlier.size = 2,
                   outlier.shape = 8,
                   outlier.colour = "black") +
      labs(title = paste0(questions_vec()[[input$variable_x_box]]),
           x = NULL, 
           y = paste0(questions_vec()[[input$variable_y_box]]),
           fill = paste0(questions_vec()[[input$variable_groupby_box]])) +
      scale_fill_manual(values = plot_cols)
    
    p1 <- 
      # categorical y
      if (input$variable_groupby_box != "None" &
          class(text.data()[[input$variable_y_box]])=="character") {
        base_plot_p1 +
          scale_x_discrete(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = y, labels = labelsy) +
          my_theme
        # numeric y 
      } else if (input$variable_groupby_box != "None" &
                 class(text.data()[[input$variable_y_box]])=="numeric") {
        base_plot_p1 +
          scale_x_discrete(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = seq(0,max(y,na.rm=T),ceiling(max(y,na.rm=T)/10))) +
          my_theme
      }
    
    # not grouped
    base_plot_p2 <- temp %>% 
      ggplot +
      aes(factor(x), y)+
      geom_boxplot(varwidth = TRUE,
                   alpha = 0.8,
                   outlier.size = 2,
                   outlier.shape = 8,
                   outlier.colour = "black",
                   fill = paletteFunc(length(levels(factor(x))))) +
      labs(title = paste0(questions_vec()[[input$variable_x_box]]),
           x = NULL, 
           y = paste0(questions_vec()[[input$variable_y_box]]))
    
    p2 <-
      # categorical y
      if (input$variable_groupby_box == "None" &
          class(text.data()[[input$variable_y_box]])=="character") {
        base_plot_p2 +
          scale_x_discrete(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = y, labels = labelsy) +
          my_theme
        
        # numeric y
      } else if (input$variable_groupby_box == "None" &
                 class(text.data()[[input$variable_y_box]])=="numeric") {
        base_plot_p2 +
          scale_x_discrete(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = seq(0,max(y,na.rm=T),ceiling(max(y,na.rm=T)/10))) +
          my_theme
      }
    
    ifelse(input$variable_groupby_box != "None",
           return(ggplotly(p1) %>% layout(boxmode = "group")),
           return(ggplotly(p2))
    )
  })
  
  ### Scatter plots  ----------
  output$scatter_plot <- renderPlotly({
    req(input$variable_groupby_scat)
    paletteFunc <- colorRampPalette(c("#18db50","#1d6cac","#00b4ff"))
    plot_cols <- paletteFunc(length(unique(num.data()[[input$variable_groupby_scat]])))
    
    x <- num.data()[[input$variable_x_scat]]
    y <- num.data()[[input$variable_y_scat]]
    g <- text.data()[[input$variable_groupby_scat]]
    labelsx = text.data()[[input$variable_x_scat]]
    labelsy = text.data()[[input$variable_y_scat]]
    temp <- data.frame(x, y)
    
    # grouped
    base_plot_p1 <- 
      ggplot(temp,aes(x, y, color = g)) +
      geom_jitter(size = 1,width=0.1,height=0.1)+
      geom_smooth(method = lm, color = "#ffa500") +
      labs(title = paste0(questions_vec()[[input$variable_x_scat]]," ~ ",questions_vec()[[input$variable_y_scat]]),
           x = paste0(questions_vec()[[input$variable_x_scat]]),
           y = paste0(questions_vec()[[input$variable_y_scat]]),
           color = paste0(questions_vec()[[input$variable_groupby_scat]]))  + 
      scale_color_manual(values = plot_cols) + 
      my_theme
    
    p1<-
      # categorical x, categorical y
      if (class(text.data()[[input$variable_x_scat]])=="character" &
          class(text.data()[[input$variable_y_scat]])=="character") {
        base_plot_p1 +
          scale_x_continuous(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = y, labels = labelsy)
        # numeric x, numeric y
      } else if (class(text.data()[[input$variable_x_scat]])=="numeric" &
                 class(text.data()[[input$variable_y_scat]])=="numeric") {
        base_plot_p1 +
          scale_x_continuous(breaks = seq(0,max(x,na.rm=T),ceiling(max(x,na.rm=T)/10))) +
          scale_y_continuous(breaks = seq(0,max(y,na.rm=T),ceiling(max(y,na.rm=T)/10)))
        # numeric x, categorical y
      } else if (class(text.data()[[input$variable_x_scat]])=="numeric" &
                 class(text.data()[[input$variable_y_scat]])=="character") {
        base_plot_p1 +
          scale_x_continuous(breaks = seq(0,max(x,na.rm=T),ceiling(max(x,na.rm=T)/10))) +
          scale_y_continuous(breaks = y, labels = labelsy) 
        # categorical x, numeric y
      } else if (class(text.data()[[input$variable_x_scat]])=="character" &
                 class(text.data()[[input$variable_y_scat]])=="numeric") {
        base_plot_p1 +
          scale_x_continuous(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = seq(0,max(y,na.rm=T),ceiling(max(y,na.rm=T)/10)))
      } 
    
    # not grouped
    base_plot_p2 <- 
      ggplot(temp,aes(x, y)) +
      geom_jitter(size = 1, color = "#00b4ff",width=0.1,height=0.1) +
      geom_smooth(method = lm, color = "#ffa500") +
      labs(title = paste0(questions_vec()[[input$variable_x_scat]]," ~ ",questions_vec()[[input$variable_y_scat]]),
           x = paste0(questions_vec()[[input$variable_x_scat]]),
           y = paste0(questions_vec()[[input$variable_y_scat]]))  + 
      my_theme
    
    p2 <- 
      # categorical x, categorical y
      if (class(text.data()[[input$variable_x_scat]])=="character" &
          class(text.data()[[input$variable_y_scat]])=="character") {
        base_plot_p2 +
          scale_x_continuous(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = y, labels = labelsy)
        # numeric x, numeric y
      } else if (class(text.data()[[input$variable_x_scat]])=="numeric" &
                 class(text.data()[[input$variable_y_scat]])=="numeric") {
        base_plot_p2 +
          scale_x_continuous(breaks = seq(0,max(x,na.rm=T),ceiling(max(x,na.rm=T)/10))) +
          scale_y_continuous(breaks = seq(0,max(y,na.rm=T),ceiling(max(y,na.rm=T)/10)))
        # numeric x, categorical y
      } else if (class(text.data()[[input$variable_x_scat]])=="numeric" &
                 class(text.data()[[input$variable_y_scat]])=="character") {
        base_plot_p2 +
          scale_x_continuous(breaks = seq(0,max(x,na.rm=T),ceiling(max(x,na.rm=T)/10))) +
          scale_y_continuous(breaks = y, labels = labelsy)
        # categorical x, numeric y
      } else if (class(text.data()[[input$variable_x_scat]])=="character" &
                 class(text.data()[[input$variable_y_scat]])=="numeric") {
        base_plot_p2 +
          scale_x_continuous(breaks = x, labels = labelsx) +
          scale_y_continuous(breaks = seq(0,max(y,na.rm=T),ceiling(max(y,na.rm=T)/10)))
      } 
    
    ifelse(input$variable_groupby_scat != "None", 
           return(ggplotly(p1)), 
           return(ggplotly(p2))
    )
  })
  
  # Map tab ----------------------------------------------------------------------
  
  ## UI elements ----------
  
  # UI TO SELECT HEATMAPS
  # Output style
  # output$style <- renderUI({
  #   radioGroupButtons(
  #     inputId = "style",
  #     label = "View data as:",
  #     choices = c("Points",
  #                 "Heatmap"),
  #     selected = "Points",
  #     size = "sm",
  #     direction = "horizontal",
  #     status = "primary"
  #   )
  # })

  # Color scale
  output$circle_color <- renderUI({
    
    `%!in%` <- Negate(`%in%`)
    if (phase() == 1) {
      choices = c(colnames(text.data()[,colnames(text.data()) %!in% c("village","gps-latitude","gps-longitude","sd11_1","w8",
                                                                      "wat5a","wat8","wat14_3","wat16","wat17","liv5","liv6")]))

    } else if (phase() == 2 ) {
      choices = c(colnames(text.data()[,colnames(text.data()) %!in% c("village","gps-latitude","gps-longitude","li8","li8_1",
                                                                      "wb9a","wb9b","wb9c","wat2a","wat8","wat9_5","wat28a","wat28b","wat29","pay4_1","pay5_1")]))
    }
      
    pickerInput("circle_color", label = "Variable to colour by:", 
                choices = choices,
                selected = "hw",
                choicesOpt = list(
                  subtext = paste(questions_vec()[1, choices])
                ),
                options = list(
                  `live-search` = TRUE)
    )
  })
  
  # Points toggle
  output$points_select <- renderUI({
    tagList(
      checkboxGroupInput("quality_toggle", "Water quality problems experienced?",
                         choices  = c(levels(factor(text.data()$wat7))),
                         selected = c(levels(factor(text.data()$wat7)))
      ),
      checkboxGroupInput("source_toggle", "Primary drinking water source:",
                         choices  = c(levels(factor(text.data()$wat1))),
                         selected = NULL
      ),
      checkboxGroupInput("hw2_toggle", "Water supply interruptions:",
                         choices  = c(levels(factor(text.data()$hw2))),
                         selected = NULL
      ),
      checkboxGroupInput("ward_toggle", "Ward:",
                         choices  = c(levels(factor(text.data()$ward))),
                         selected = NULL
      )
    )
  })
  
  
  ## Plotting groups ----------
  
  # Filter data based on toggles
  filtered_data <- eventReactive({
    input$quality_toggle
    input$hw2_toggle
    input$source_toggle
    input$ward_toggle}, {
      
      results <- text.data() %>% {
        if(!is.null(input$quality_toggle)) {
          filter(.,wat7 %in% input$quality_toggle)
        } else {
          filter(.,TRUE)
        }
      } %>% {
        if(!is.null(input$hw2_toggle)) {
          filter(.,hw2 %in% input$hw2_toggle)
        } else {
          filter(.,TRUE)
        }
      } %>% {
        if(!is.null(input$source_toggle)) {
          filter(.,wat1 %in% input$source_toggle)
        } else {
          filter(.,TRUE)
        } 
      } %>% {
        if(!is.null(input$ward_toggle)) {
          filter(.,ward %in% input$ward_toggle)
        } else {
          filter(.,TRUE)
        } 
      }
      if(is.null(input$source_toggle) & is.null(input$hw2_toggle) & is.null(input$quality_toggle) & is.null(input$ward_toggle)){
        results <- text.data() %>% filter(FALSE)
      }
      return(results)
    }, ignoreNULL = FALSE)
  
  
  ## Map Layers ----------
  
  # Read shape files
  # unions
  file<-tempdir()
  unzip("shapefiles/union/Unions.zip", exdir=file)
  unions <- st_read(dsn = file, layer = "Unions")

  # read infrastructure data
  file1<-tempdir()
  unzip("shapefiles/infra/WASH_Infras_LT_Bath_TW_May_31_2022A.zip", exdir=file1)
  unzip("shapefiles/infra/WASH_Infras_LT_Bath_TW_May_31_2022B.zip", exdir=file1)
  infra <- st_read(dsn = file1, layer = "WASH_Infras_LT_Bath_TW_May_31_2022")
  infra <- subset(infra, infra$Upazila=="Teknaf") %>% infra[c(1:33)]
  #replacements for consistency
  infra$Type_Faci <- sapply(infra$Type_Faci, function(x) {
    x = replace(x, which(x=="latrine"),"Latrine")
    x = replace(x, which(x=="Tubewell-Handpump"), "Handpump Tubewell")
    x = replace(x, which(x=="Both (Latrine & Bathing)"), "Latrine & Bathing")
  })
  
  # camps
  file2<-tempdir()
  unzip("shapefiles/camp/T220130_RRC_Outline_Camp_AL1.zip", exdir=file2)
  camps <- st_read(dsn = file2, layer = "T220130_RRC_Outline_Camp_AL1")
  camps <- subset(camps, camps$Upazila=="Teknaf")
  
  ## Base map creation ----
  output$mymap <- renderLeaflet({
    
    leaflet(data = text.data()) %>% 
      setView(lng = text.data()[1, "gps-longitude"],
              lat = text.data()[1, "gps-latitude"],
              zoom=12) %>%
      addTiles(group = "Default Map") %>% 
      addScaleBar(position = "bottomleft",
                  options = scaleBarOptions(maxWidth=400)) %>%
      addLogo(img = "north.png",
              src = "remote",
              position = "bottomleft",
              height = 100,
              width = 55,
              offset.y = 80) %>%
      addMiniMap(position = "bottomright", toggleDisplay = TRUE, width = 200) %>%
      clearGroup("points") %>%
      
      ## Infrastructure ----   
    addCircleMarkers(data = infra[infra$Type_Faci=="Bathing Cubicle" | infra$Type_Faci=="Latrine & Bathing", ],
                     ~Long,
                     ~Lat,
                     radius = 3,
                     stroke = T,
                     weight = 1, opacity = 1,
                     fillOpacity = 0,
                     group = "Bathing Cubicle",
                     popup=~paste(
                       "<b>", "Agency: ",Agency,"</b><br/>",
                       "Facility: ", Type_Faci, "<br/>",
                       "Sub Type: ",Sub_Type_F, "<br/>",
                       "Bathing Total: ", Bathing, "<br/>",
                       "Female: ", Bathing_F, "<br/>",
                       "Male: ", Bathing_M, "<br/>",
                       "Universal: ", Bath_gen_u, "<br/>"))%>%
      addCircleMarkers(data = infra[infra$Type_Faci=="Latrine" | infra$Type_Faci=="Latrine & Bathing", ],
                       ~Long,
                       ~Lat,
                       radius = 3,
                       stroke = T,
                       weight = 1, opacity = 1,
                       fillOpacity = 0,
                       color="green",
                       group = "Latrine",
                       popup=~paste(
                         "<b>", "Agency: ",Agency,"</b><br/>",
                         "Facility: ", Type_Faci, "<br/>",
                         "Sub Type: ",Sub_Type_F, "<br/>",
                         "Latrines: ", LT, "<br/>",
                         "Female: ", LT_F, "<br/>",
                         "Male: ", LT_M, "<br/>",
                         "Universal: ", LT_Gen_uns, "<br/>",
                         "Slabs: ", Slabs, "<br/>",
                         "Rings: ", Rings, "<br/>",
                         "Structure: ", struc_wall, ", ", struc_pill, "<br/>",
                         "Volume (m3): ", Volume_M3, "<br/>"))%>%
      addCircleMarkers(data = infra[infra$Type_Faci=="Handpump Tubewell", ],
                       ~Long,
                       ~Lat,
                       radius = 3,
                       stroke = T,
                       weight = 1, opacity = 1,
                       fillOpacity = 0,
                       color="red",
                       group = "Handpump Tubewell",
                       popup=~paste(
                         "<b>", "Agency: ",Agency,"</b><br/>",
                         "Facility: ", Type_Faci, "<br/>",
                         "Sub Type: ",Sub_Type_F, "<br/>",
                         "TW_Depth: ", Depth_TW_F, "<br/>"))%>%
      addPolygons(data = camps,
                  color = "#800080", weight = 2, smoothFactor = 0.5, opacity = 1, 
                  fill= FALSE, group = "Camp Boundaries") %>%
      addPolygons(data = unions,
                  color = "#cc0000", weight = 2, smoothFactor = 0.5, opacity = 1, 
                  fill= FALSE, group = "Union Boundaries") %>%
      addLayersControl(baseGroups = c("Default Map","Satelite Map", "Minimal Map"),
                       overlayGroups = c("Camp Boundaries", "Union Boundaries", "Bathing Cubicle", "Latrine", "Handpump Tubewell"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Bathing Cubicle", "Latrine", "Handpump Tubewell"))
  })
  
  # Dynamic points ----
  observeEvent({
    filtered_data()
    input$circle_color
    #input$style
  }, {
    # set palette
    pal1 <- colorRampPalette(c("#8e0000","#ff1a1a","#ffa500","#ffd700","#00ff00","#007300","#00ced1","#0066d1","#003366"))

    # Manage legend
    # circle color variable: numeric
    if (class(text.data()[[input$circle_color]])=="numeric") {
      colorData <- filtered_data()[[input$circle_color]]
      color <- colorNumeric(rev(pal1(9)), domain = text.data()[[input$circle_color]])
      legend_vals = text.data()[[input$circle_color]]

    } else {
      # "often" scale
      if ("Always" %in% filtered_data()[[input$circle_color]]) {
        levels = c("Always","Often","Sometimes","Rarely","Never")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(5), levels = levels)
        legend_vals = colorData
        
        # "important" scale
      } else if ("Somewhat Important" %in% filtered_data()[[input$circle_color]] | "Neither Imp Nor Unimportant" %in% filtered_data()[[input$circle_color]] ) {
        levels = c("Very Important","Somewhat Important","Neither Imp Nor Unimportant","Not Important At All")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(4), levels = levels)
        legend_vals = colorData
        
        # "satisfied" scale
      } else if ("Somewhat Satisfied" %in% filtered_data()[[input$circle_color]] | "Somewhat Dissatisfied" %in% filtered_data()[[input$circle_color]] ) {
        levels = c("Very Satisfied","Somewhat Satisfied","Somewhat Dissatisfied","Very Dissatisfied")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(4), levels = levels)
        legend_vals = colorData
        
        # "often" scale
      } else if ("Sometimes" %in% filtered_data()[[input$circle_color]] | "Fairly Often" %in% filtered_data()[[input$circle_color]] ) {
        levels = c("Very Often","Fairly Often","Sometimes","Almost Never", "Never")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(5), levels = levels)
        legend_vals = colorData
        
        # "good" scale
      } else if ("Fair" %in% filtered_data()[[input$circle_color]] | "Good" %in% filtered_data()[[input$circle_color]] ) {
        levels = c("Poor","Fair","Good","Very Good", "Excellent")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(5), levels = levels)
        legend_vals = colorData
        
        # "half" scale
      } else if ("Half" %in% filtered_data()[[input$circle_color]]) {
        levels = c("None","Some","Half","Most", "All")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(5), levels = levels)
        legend_vals = colorData
        
        # month order
      } else if ("Jul" %in% filtered_data()[[input$circle_color]]) {
        levels = format(ISOdatetime(2000,1:12,1,0,0,0),"%b")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(pal1(12), levels = levels)
        legend_vals = colorData
        
      } else if (input$circle_color == "wat5b") {
        levels = c("0-100 Mins","101-200 Mins","201-300 Mins","301-400 Mins","401-500 Mins","501-600 Mins","600+ Mins")
        colorData <- factor(filtered_data()[[input$circle_color]],levels = levels)
        color <- colorFactor(rev(pal1(7)), levels = levels)
        legend_vals = colorData
        
        # all other categorical vars
      } else {
        colorData <- filtered_data()[[input$circle_color]]
        color = colorFactor(pal1(length(unique(colorData))),domain = colorData)
        legend_vals = colorData
      }
    }
    
    
    # (DRY) ----
    # add to if for heatmaps: input$style == "Points" & 
    if (phase() == 1 ) {
      leafletProxy("mymap") %>%
        clearGroup("points") %>%
        addProviderTiles("CartoDB.Positron", group = "Minimal Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satelite Map") %>%
        removeControl("colorLegend") %>%
        addCircleMarkers(data = filtered_data(),
                         filtered_data()[, "gps-longitude"],
                         filtered_data()[, "gps-latitude"],
                         radius = 4,
                         stroke = FALSE,
                         group = "points",
                         fillOpacity=1,
                         popup = ~paste(
                           "<b><font color='#00b4ff'>", "Union: ","</font></b>",union,"<b><font color='#00b4ff'>", " Ward: ","</font></b>",ward,"<br/>",
                           "<b><font color='#00b4ff'>", "Camp: ","</font></b>",camp,"<b><font color='#00b4ff'>", " Block: ","</font></b>",block,"<br/>",
                           "<b><font color='#00b4ff'>", "Household members: ","</font></b>",sd6_+sd7+sd8,"<br/>",
                           "<b><font color='#00b4ff'>", "HWISE score: ","</font></b>",hw,"<br/>",
                           "<b><font color='#00b4ff'>","Primary source (drinking): ","</font></b>",wat1, "<br/>",
                           "<b><font color='#00b4ff'>","Primary source (non-drinking): ","</font></b>",wat3, "<br/>",
                           "<b><font color='#00b4ff'>","Water quality problems: ","</font></b>",wat7, "<br/>",
                           "<b><font color='#00b4ff'>","Nature of problem: ","</font></b>",wat8, "<br/>",
                           "<b><font color='#00b4ff'>","Supply interruptions: ","</font></b>",hw2, "<br/>",
                           "<b><font color='#00b4ff'>","Supply fee structure: ","</font></b>",pay3,"<br/>"
                         ),
                         color = ~color(colorData)) %>%
        addLegend("topright",
                  pal=color,
                  values=legend_vals,
                  opacity = 1,
                  title=questions_vec()[[input$circle_color]],
                  layerId="colorLegend")
      
    # PHASE 1 HEATMAP
    # } else if (input$style == "Heatmap" & phase() == 1) {
    #   leafletProxy("mymap",data = num.data()) %>%
    #     clearGroup("points") %>%
    #     addProviderTiles("CartoDB.Positron", group = "Minimal Map") %>%
    #     addProviderTiles("Esri.WorldImagery", group = "Satelite Map") %>%
    #     removeControl("colorLegend") %>%
    # 
    #     addHeatmap(data=num.data(),
    #                num.data()[, "gps-longitude"],
    #                num.data()[, "gps-latitude"],
    #                group = "points", blur = 35, cellSize = 1, radius = 10,
    #                minOpacity = 0,
    #                intensity = num.data()[[input$circle_color]])
    # (WET) ----
      # add to if for heatmaps: input$style == "Points" & 
    } else if (phase() == 2) {
      leafletProxy("mymap", data = filtered_data()) %>%
        clearGroup("points") %>%
        addProviderTiles("CartoDB.Positron", group = "Minimal Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satelite Map") %>%

        removeControl("colorLegend") %>%
        addCircleMarkers(data = filtered_data(),
                         filtered_data()[, "gps-longitude"],
                         filtered_data()[, "gps-latitude"],
                         radius = 4,
                         stroke = FALSE,
                         group = "points",
                         fillOpacity=1,
                         popup = ~paste(
                           "<b><font color='#00b4ff'>", "Union: ","</font></b>",union,"<b><font color='#00b4ff'>", " Ward: ","</font></b>",ward,"<br/>",
                           "<b><font color='#00b4ff'>", "Camp: ","</font></b>",camp,"<b><font color='#00b4ff'>", " Block: ","</font></b>",block,"<br/>",
                           "<b><font color='#00b4ff'>", "Household members: ","</font></b>",sd6_+sd7+sd8,"<br/>",
                           "<b><font color='#00b4ff'>", "HWISE score: ","</font></b>",hw,"<br/>",
                           "<b><font color='#00b4ff'>","Primary source (drinking): ","</font></b>",wat1, "<br/>",
                           "<b><font color='#00b4ff'>","Primary source (non-drinking): ","</font></b>",wat10, "<br/>",
                           "<b><font color='#00b4ff'>","Drinking water litres per day: ","</font></b>",wat6, "<br/>",
                           "<b><font color='#00b4ff'>","Water quality problems: ","</font></b>",wat7, "<br/>",
                           "<b><font color='#00b4ff'>","Nature of problem: ","</font></b>",wat8, "<br/>",
                           "<b><font color='#00b4ff'>","Supply interruptions: ","</font></b>",hw2, "<br/>",
                           "<b><font color='#00b4ff'>","Pay for water supply: ","</font></b>",pay1, "<b><font color='#00b4ff'>", " Amount paid: ","</font></b>",pay2,"<br/>"
                         ),
                         color = ~color(colorData)) %>%
        addLegend("topright",
                  pal=color,
                  values=legend_vals,
                  opacity = 1,
                  title=questions_vec()[[input$circle_color]],
                  layerId="colorLegend"
        ) 
    # PHASE 2 HEATMAP
    # } else if (input$style == "Heatmap" & phase() == 2) {
    #   leafletProxy("mymap", data = num.data()) %>%
    #     clearGroup(c("points")) %>%
    #     addProviderTiles("CartoDB.Positron", group = "Minimal Map") %>%
    #     addProviderTiles("Esri.WorldImagery", group = "Satelite Map") %>%
    #     removeControl("colorLegend") %>%
    #     
    #     addHeatmap(data=num.data(),
    #                num.data()[, "gps-longitude"],
    #                num.data()[, "gps-latitude"],
    #                group = "points", blur = 35, cellSize = 1, radius = 10,
    #                minOpacity = 0,
    #                intensity = num.data()[[input$circle_color]]
    #     ) 
    }
  })
  
  # Survey Q tab  ----------------------------------------------------------------------
  
  ## UI elements ----------
  output$q_select <- renderUI({
    if (phase() == 1) {
      checkboxGroupInput(
        "q_select", "Select survey domains to display:",
        choices = c("Location","Union","Ward","Village","Camp","Block","Sex",
                    "Sociodemographic","Work","Perception of Water Services",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Livelihood Activities","Paying for Water","Well-being"),
        selected = "HWISE"
      )
    } else if (phase() == 2) {
      checkboxGroupInput(
        "q_select", "Select survey domains to display:",
        choices = c("Location","Union","Ward","Village","Camp","Block","Sex",
                    "Sociodemographic","Livelihood & Income",
                    "HWISE","Water Access, Quantity, Utility, Stability",
                    "Paying for Water","Well-being"),
        selected = "HWISE" 
      )
    }
  })
  
  ## Create data objects ----
  filtered_Qdata <- reactive({
    if(phase() == 1) {
      varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(questions_vec()[, grepl("sd", names(questions_vec()))])),
        "Work"=c("w1_1","w2","w3","w4","w5","w6","w7","w8"),
        "Perception of Water Services"=c("p1","p2","p3","p4","p5"),
        "HWISE"=c(colnames(questions_vec()[, grepl("hw", names(questions_vec()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(questions_vec()[, grepl("wat", names(questions_vec()))])),
        "Livelihood Activities"=c(colnames(questions_vec()[, grepl("liv", names(questions_vec()))])),
        "Paying for Water"=c(colnames(questions_vec()[, grepl("pay", names(questions_vec()))])),
        "Well-being"=c(colnames(questions_vec()[, grepl("wb", names(questions_vec()))]))
      )
    } else if (phase() == 2) {
      varGroups <- list(
        "Location" = "location",
        "Union" = "union",
        "Ward" = "ward",
        "Village" = "village",
        "Camp" = "camp",
        "Block" = "block",
        "Sex"="sex",
        "Sociodemographic"=c(colnames(text.data()[, grepl("sd", names(text.data()))])),
        "Livelihood & Income"=c(colnames(text.data()[, grepl("li", names(text.data()))])),
        "HWISE"=c(colnames(text.data()[, grepl("hw", names(text.data()))])),
        "Water Access, Quantity, Utility, Stability"=c(colnames(text.data()[, grepl("wat", names(text.data()))])),
        "Paying for Water"=c(colnames(text.data()[, grepl("pay", names(text.data()))])),
        "Well-being"=c(colnames(text.data()[, grepl("wb", names(text.data()))]))
      )
    }
    
    names <- unlist(lapply(input$q_select, function(x) varGroups[[x]]))
    df <- questions_vec() %>% select(all_of(names))
    df <- data.frame(t(df)) %>% 
      rename("Question" = "X2")
    df$`Example Responses` = apply(data.frame(text.data()[, colnames(text.data()) %in% rownames(df)]), 
                                   2, 
                                   function(x) list(c(levels(factor(x)))))
    df$`Example Responses` = sapply( df$`Example Responses`, function(x) if(length(unlist(x))>55) {
      x = "Free text response"
    } else if(length(unlist(x))>5) { 
      x = list(c(head(unlist(x), n=3), "..."))
    } else {
      x = x
    }
    )
    return(df)
    
  })
  
  ## render output
  output$filtered_Qdata <- renderDataTable(
    filtered_Qdata(), rownames=TRUE,
    options = list(pageLength = 25)
  )
  
  # Instructions tab  ----------------------------------------------------------
  
  ## UI elements ----------
  output$doc_select <- renderUI({
    radioGroupButtons(
      inputId = "doc_select",
      label = "View instructions:",
      choices = c("Navigation",
                  "Survey Questions",
                  "Summary Stats",
                  "Map",
                  "Plots",
                  "Data"),
      selected = "Navigation",
      size = "normal",
      direction = "vertical",
      status = "primary"
    )
  })
  
  ## Render ----
  output$doc_image <- renderImage({
    
    filename <- file.path("www/documentation", winslash = "/",
                          paste(input$doc_select, ".png", sep = ""))
    
    list(src = filename, width="90%")
    
  }, deleteFile = FALSE)
  
  
})  
shinyApp(ui,server)

