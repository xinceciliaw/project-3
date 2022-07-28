
library(shinydashboard)
library(shiny)

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
                                       "La Leche League" = "la_leche_in_county",
                                       "Baby-Friendly Hospital" = "baby_friendly_in_county",
                                       "WIC program site" = "wic_site_in_county",
                                       "Rural-Urban Continuum Code" = "rucc"),
                                     selected = "la_leche_in_county")
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
                                                    "Contingency Table")),
                         conditionalPanel(
                           condition = "input.numsumary == 'Summary Statistics'",
                           selectInput("fivenum",
                                       "Select a variable",
                                       choices = list(
                                         "Breastfeeding Initiation" = "bf_pct",
                                         "Infant Mortality" = "im_rate",
                                         "IBCLC" = "ibclc_rate",
                                         "La Leche League" = "la_leche_in_county",
                                         "Baby-Friendly Hospital" = "baby_friendly_in_county",
                                         "WIC program site" = "wic_site_in_county",
                                         "Rural-Urban Continuum Code" = "rucc",
                                         "Social Vulnerability Index" = "svi"),
                                       selected = "bf_pct")
                         ),
                         conditionalPanel(
                           condition = "input.numsumary == 'Frequency Table'",
                           selectInput("Frequency",
                                       "Select a variable",
                                       choices = list(
                                         "La Leche League" = "la_leche_in_county",
                                         "Baby-Friendly Hospital" = "baby_friendly_in_county",
                                         "WIC program site" = "wic_site_in_county",
                                         "Rural-Urban Continuum Code" = "rucc",
                                         "Social Vulnerability Index" = "svi"),
                                       selected = "bf_pct")
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
                         ))
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
              tabPanel("Model Fitting"),
              tabPanel("Model Prediction")
            )),
    tabItem(tabName = "data")
  )
)

dashboardPage(
  dashboardHeader(title = "Breastfeeding Data Analysis",
                  titleWidth = 360),
  sidebar,
  body
)
