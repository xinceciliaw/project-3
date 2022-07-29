
library(shinydashboard)
library(shiny)
library(DT)
library(plotly)

# create sidebar 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Data Exploration", tabName = "explore"),
    menuItem("Modeling", tabName = "modeling"),
    menuItem("Data", tabName = "data")
  )
)

# create body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            tabsetPanel(
              tabPanel("Purpose",
                       h3("The purpose of the app is to explore breastfeeding data, which allow users to: "),
                       h4(tags$ul(
                         tags$li("Do data exploration"), 
                         tags$li("Fit three supervised learning models"), 
                         tags$li("Use one of the models for prediction")))
              ),
              tabPanel("Introduction",
                       h3("About"),
                       h4("The About page describes the purpose of the app, and introduces the data used for the analysis."),
                       br(),
                       h3("Data Exploration"),
                       h4("allow users to create numerical and graphical summaries, blabla"),
                       br(),
                       h3("Modeling"),
                       h4("blabla"),
                       br(),
                       h3("Data"),
                       h4("The Data page allow users to scroll through the data set, subset the data set and download the subsetted data as a csv file.")

                       ),
              tabPanel("Data",
                       h3("Data Introduction"),
                       h4("blabla"),
                       br(),
                       h3("Variables and Sources"),
                       h4(tags$ul(
                         tags$li(tags$b("Breastfeeding Initiation")," : % of live births, from", tags$a(href="https://www.cdc.gov/breastfeeding/data/county/2018-2019/georgia.html", "the National Vital Statistics System of the CDC’s National Center for Health Statistics.")), 
                         tags$li(tags$b("Infant Mortality")," : rate per 1K live births, from", tags$a(href="https://oasis.state.ga.us/oasis/webquery/qryInfantDeath.aspx", "the Georgia Department of Public Health and the North Carolina Institute of Medicine.")), 
                         tags$li(tags$b("IBCLC")," : lactation consultants per 1K live births, from", tags$a(href="https://www.alpp.org/search", "the Academy of Lactation Policy and Practice.")),
                         tags$li(tags$b("La Leche League")," : mother-to-mother support, from", tags$a(href="https://lllusa.org/locator/", "La Leche League USA’s website.")),
                         tags$li(tags$b("Baby-Friendly Hospital")," : designated hospitals, from", tags$a(href="https://www.babyfriendlyusa.org/for-parents/find-a-baby-friendly-facility/", "Baby-Friendly USA’s website.")),
                         tags$li(tags$b("WIC program site")," : Women & Infant Children Program, from", tags$a(href="https://www.zipmilk.org/states/georgia?zip=31518", "the Massachusetts Breastfeeding Coalition.")),
                         tags$li(tags$b("Rural-Urban Continuum Code")," : ranges from 1 (most urban) to 9 (most rural), from", tags$a(href="https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx", "US Department of Agriculture’s Economic Research Service.")),
                         tags$li(tags$b("Social Vulnerability Index")," : ranges from 0 (the least vulnerable county) to 100 (the most vulnerable county), from", tags$a(href="https://www.atsdr.cdc.gov/placeandhealth/svi/index.html", "US Centers for Disease Control and Prevention’s Agency for Toxic Substances and Disease Registry.")),
                         ))
                       ),

              tabPanel("Image",
                       img(src = "picture.jpeg", width = "80%"))
              
              )),

    tabItem(tabName = "explore",
            fluidRow(
              column(3,
                     box(width = 12, 
                         title = "Graphical Summaries", 
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = TRUE,
                       selectInput("plots",
                                   "Select Plot Type",
                                   choices = list("Density",
                                                  "Bar")),
                       conditionalPanel(
                         condition = "input.plots == 'Density'",
                         selectInput("densityx",
                                     "Select a variable",
                                     choices = list(
                                       "Breastfeeding Initiation" = "bf_pct",
                                       "Infant Mortality" = "im_rate",
                                       "IBCLC" = "ibclc_rate",
                                       "Rural-Urban Continuum Code" = "rucc",
                                       "Social Vulnerability Index" = "svi"),
                                     selected = "ibclc_rate")),
                       conditionalPanel(
                         condition = "input.plots == 'Bar'",
                         selectInput("barx",
                                     "Select a variable",
                                     choices = list(
                                       "La Leche League" = "la_leche_count",
                                       "Baby-Friendly Hospital" = "baby_friendly_count",
                                       "WIC program site" = "wic_site_count",
                                       "Rural-Urban Continuum Code" = "rucc"),
                                     selected = "la_leche_count")
                       )
                     ),
                     box(width = 12, 
                         title = "Numerical Summaries", 
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = TRUE,
                         selectInput("numsumary",
                                     "Select Summary Type",
                                     choices = list("Summary Statistics",
                                                    "Correlation Table")),
                         conditionalPanel(
                           condition = "input.numsumary == 'Summary Statistics'",
                           selectInput("fivenum",
                                       "Select a variable",
                                       choices = list(
                                         "Breastfeeding Initiation" = "bf_pct",
                                         "Infant Mortality" = "im_rate",
                                         "IBCLC" = "ibclc_rate",
                                         "La Leche League" = "la_leche_count",
                                         "Baby-Friendly Hospital" = "baby_friendly_count",
                                         "WIC program site" = "wic_site_count",
                                         "Rural-Urban Continuum Code" = "rucc",
                                         "Social Vulnerability Index" = "svi"),
                                       selected = "bf_pct")
                         ),
                         conditionalPanel(
                           condition = "input.numsumary == 'Correlation Table'",
                           checkboxGroupInput("Correlation",
                                       "Select Variable(s) to See Correlation",
                                       choices = list(
                                         "Breastfeeding Initiation" = "bf_pct",
                                         "Infant Mortality" = "im_rate",
                                         "IBCLC" = "ibclc_rate",
                                         "La Leche League" = "la_leche_count",
                                         "Baby-Friendly Hospital" = "baby_friendly_count",
                                         "WIC program site" = "wic_site_count",
                                         "Rural-Urban Continuum Code" = "rucc",
                                         "Social Vulnerability Index" = "svi"),
                                       selected = list(
                                         "Breastfeeding Initiation" = "bf_pct",
                                         "IBCLC" = "ibclc_rate",
                                         "La Leche League" = "la_leche_count"
                                       ))
                         )
                         
                         ),
                     box(width = 12, 
                         title = "Filter by Variables", 
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = TRUE,
                         checkboxInput("filtervar_bi",
                                       "Filter by Range of Breastfeeding Initiation",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_bi == 1",
                           sliderInput("explore_filtervar_bi",
                                       label = "Breastfeeding Initiation (%)",
                                       min = 0, max = 100, value = c(0, 100))
                         ),
                         checkboxInput("filtervar_im",
                                       "Filter by Range of Infant Mortality",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_im == 1",
                           sliderInput("explore_filtervar_im",
                                       label = "Infant Mortality (per 1K)",
                                       min = 0, max = 40, value = c(0, 40))
                         ),
                         checkboxInput("filtervar_ibclc",
                                       "Filter by Range of IBCLC",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_ibclc == 1",
                           sliderInput("explore_filtervar_ibclc",
                                       label = "IBCLC (per 1K)",
                                       min = 0, max = 50, value = c(0, 50))
                         ),
                         checkboxInput("filtervar_laleche",
                                       "Filter by Range of La-Leche League",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_laleche == 1",
                           sliderInput("explore_filtervar_laleche",
                                       label = "La Leche League (count)",
                                       min = 0, max = 11, value = c(0, 11))
                         ),
                         checkboxInput("filtervar_babyhospitcal",
                                       "Filter by Range of Baby-Friendly Hospital",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_babyhospitcal == 1",
                           sliderInput("explore_filtervar_babyhospitcal",
                                       label = "Baby-Friendly Hospital (count)",
                                       min = 0, max = 7, value = c(0, 7))
                         ),
                         checkboxInput("filtervar_wic",
                                       "Filter by Range of WIC program site",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_wic == 1",
                           sliderInput("explore_filtervar_wic",
                                       label = "WIC program site (count)",
                                       min = 0, max = 15, value = c(0, 15))
                         ),
                         checkboxInput("filtervar_rucc",
                                       "Filter by Range of Rural-Urban Continuum Code",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_rucc == 1",
                           sliderInput("explore_filtervar_rucc",
                                       label = "Rural-Urban Continuum Code",
                                       min = 1, max = 9, value = c(1, 9))
                         ),
                         checkboxInput("filtervar_svi",
                                       "Filter by Range of Social Vulnerability Index",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.filtervar_svi == 1",
                           sliderInput("explore_filtervar_svi",
                                       label = "Social Vulnerability Index",
                                       min = 0, max = 100, value = c(0, 100))
                         )
                         )),
              column(9,
                     box(width = 12,
                         plotlyOutput(
                           outputId = "explore_plot"
                         ))
                     ),
              column(9,
                     h4("Numerical Summaries"),
                     dataTableOutput(outputId = "explore_numerical_summary"))
            )),
    tabItem(tabName = "modeling",
            tabsetPanel(
              tabPanel("Model Introduction",
                       h3("Multiple Linear Regression Model"),
                       h4("blablabla"),
                       br(),
                       h3("Regression Tree Model"),
                       h4("more blabla"),
                       br(),
                       h3("Random Forest Model"),
                       h4("bllaaa")),
              tabPanel("Model Fitting",
                       fluidRow(
                         column(3,
                                box(width = 12, 
                                    title = "Modeling", 
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    sliderInput("trainset",
                                                "Training Data set",
                                                min = 10, max = 100, step = 10, value = 20),
                                    checkboxGroupInput(
                                      "select_predictor",
                                      "Select Predictor Variables",
                                      choices = c("IBCLC" = "ibclc_rate",
                                                  "La Leche League" = "la_leche_count",
                                                  "Baby-Friendly Hospital" = "baby_friendly_count",
                                                  "WIC program site" = "wic_site_count",
                                                  "Rural-Urban Continuum Code" = "rucc",
                                                  "Social Vulnerability Index" = "svi"),
                                      selected = list("Breastfeeding Initiation" = "bf_pct",
                                                      "IBCLC" = "ibclc_rate")
                                    )
                                    ),
                                box(width = 12, 
                                    title = "Model Type", 
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    checkboxInput(
                                      inputId = "model_mlr",
                                      label = "Multiple Linear Regression",
                                      value = TRUE),
                                    checkboxInput(inputId = "model_regtree",
                                                  label = "Regression Tree",
                                                  value = TRUE),
                                    conditionalPanel(
                                      condition = "input.model_regtree == 1",
                                      numericInput("cp",
                                                   "Complexity Parameter",
                                                   value = 0.01,
                                                   min = 0.001, max = 0.1, 
                                                   step = 0.005)),
                                    checkboxInput(inputId = "model_rf",
                                                  label = "Random Forest",
                                                  value = TRUE),
                                    conditionalPanel(
                                      condition = "input.model_rf == 1",
                                      sliderInput(inputId = "mtry",
                                                  label = "mtry",
                                                  min = 1, max = 6, value = 2, 
                                                  step = 1)),
                                    conditionalPanel(
                                      condition = "(input.model_mlr | input.model_regtree | input.model_rf) == 1",
                                      checkboxInput(inputId = "cv",
                                                    label = "Cross Validation",
                                                    value = TRUE),
                                      conditionalPanel(
                                        condition = "input.cv == 1",
                                        sliderInput(inputId = "folds",
                                                    label = "Folds",
                                                    min = 2, max = 10, value = 5))
                                    )
                                    ),
                                box(width = 12,
                                    actionButton(inputId = "submit_models",
                                                 label = "Fit on Training Set"))
                                ), #finish col3
                         column(9,
                                box(width = 12,
                                    status = "primary",
                                    h3("Training Data RMSE"),
                                    tableOutput(outputId = "rmse_training_all_model")), # end box
                                box(width = 12, 
                                    solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    title = "Training Data Outputs",
                                    h4("Multiple Linear Regression"),
                                    verbatimTextOutput("result_training_mlr"),
                                    h4("Regression Tree"),
                                    verbatimTextOutput("result_training_tree"),
                                    h4("Random Forest"),
                                    verbatimTextOutput("result_training_rf")),# end box
                                br(),
                                h3("Variable Importance Measure"),
                                h4("Random Forest"),
                                conditionalPanel(
                                  condition = "input.model_rf == 1",
                                  plotOutput(outputId = "summary_rf")
                                ),
                                box(width = 12, 
                                    status = "primary",
                                    h3("Test Data RMSE"),
                                    tableOutput(outputId = "rmse_test_all_model")) # end box

                                )
                       )),
              tabPanel("Model Prediction",
                       fluidRow(
                         column(4,
                                box(width = 12,
                                    title = "Predict Breastfeeding Initiation Rate",
                                    solidHeader = TRUE,
                                    numericInput(inputId = "predict_ibclc",
                                                 label = "IBCLC (per 1K)",
                                                 value = 5.5,
                                                 min = 0, max = 46.7),
                                    numericInput(inputId = "predict_la_leche",
                                                 label = "La Leche League (count)",
                                                 value = 2,
                                                 min = 0, max = 11),
                                    numericInput(inputId = "predict_babyhospital",
                                                 label = "Baby-Friendly Hospital (count)",
                                                 value = 1,
                                                 min = 0, max = 7),
                                    numericInput(inputId = "predict_wic",
                                                 label = "WIC program site (count)",
                                                 value = 2,
                                                 min = 0, max = 15),
                                    numericInput(inputId = "predict_rucc",
                                                 label = "Rural-Urban Continuum Code",
                                                 value = 3,
                                                 min = 1, max = 9),
                                    numericInput(inputId = "predict_svi",
                                                 label = "Social Vulnerability Index",
                                                 value = 3,
                                                 min = 3.54, max = 99.4),
                                    selectInput(
                                      inputId = "predict_select_model",
                                      label = "Select Model for Prediction",
                                      choices = c("Multiple Linear Regression",
                                                  "Regression Tree",
                                                  "Random Forest"),
                                      selected = "Multiple Linear Regression"),
                                    actionButton(
                                      inputId = "submit_predict",
                                      label = "Make Prediction")

                                ) #end box
                                ),
                                column(8,
                                       box(width = 12,
                                           h3("Prediction Result"),
                                           tableOutput(outputId = "predict_value_table"))
                                )
                       ))
            )),
    tabItem(tabName = "data",
            fluidRow(
              column(3,
                     box(width = 12,
                         title = "Select Variables",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         checkboxGroupInput("select_var",
                                            "Selection",
                                            choices = list(
                                              "Breastfeeding Initiation" = "bf_pct",
                                              "Infant Mortality" = "im_rate",
                                              "IBCLC" = "ibclc_rate",
                                              "La Leche League" = "la_leche_count",
                                              "Baby-Friendly Hospital" = "baby_friendly_count",
                                              "WIC program site" = "wic_site_count",
                                              "Rural-Urban Continuum Code" = "rucc",
                                              "Social Vulnerability Index" = "svi"),
                                            selected = list(
                                              "Breastfeeding Initiation" = "bf_pct",
                                              "Infant Mortality" = "im_rate",
                                              "IBCLC" = "ibclc_rate",
                                              "La Leche League" = "la_leche_count"))),
                     box(width = 12,
                         title = "Filter Variables",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         sliderInput(
                           "filter_bi",
                           "Breastfeeding Initiation (%)",
                           min = 0, max = 100, value = c(0, 100)),
                         sliderInput(
                           "filter_im",
                           "Infant Mortality (per 1K)",
                           min = 0, max = 40, value = c(0, 40)),
                         sliderInput(
                           "filter_ibclc",
                           "IBCLC (per 1K)",
                           min = 0, max = 50, value = c(0, 50)),
                         sliderInput(
                           "filter_laleche",
                           "La Leche League (count)",
                           min = 0, max = 15, value = c(0, 15)),
                         sliderInput(
                           "filter_babyhospital",
                           "Baby-Friendly Hospital (count)",
                           min = 0, max = 10, value = c(0, 10)),
                         sliderInput(
                           "filter_wic",
                           "WIC program site (count)",
                           min = 0, max = 15, value = c(0, 15)),
                         sliderInput(
                           "filter_rucc",
                           "Rural-Urban Continuum Code",
                           min = 1, max = 9, value = c(1, 9)),
                         sliderInput(
                           "filter_svi",
                           "Social Vulnerability Index",
                           min = 0, max = 100, value = c(0, 100)),
                         )),
              column(9,
                     DTOutput(outputId = "dt_table"),
                     br(),
                     downloadButton(outputId = "dt_download", label = "Data Download"))
            )
            )
  )
)

dashboardPage(
  dashboardHeader(title = "Breastfeeding Data Analysis",
                  titleWidth = 360),
  sidebar,
  body
)
