

library(shiny)
library(DT)
library(knitr)
library(caret)
library(tree)
library(randomForest)
library(tidyverse)


dat <- read_csv("breastfeeding.csv")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    get_dat <- reactive({
      data <- dat %>% 
      filter(
        (bf_pct <= (input$filter_bi[2])) &
          (bf_pct >= (input$filter_bi[1]))) %>%
      filter(
        (im_rate <= (input$filter_im[2])) &
          (im_rate >= (input$filter_im[1]))) %>% 
      filter(
        (ibclc_rate <= (input$filter_ibclc[2])) & 
          (ibclc_rate >= (input$filter_ibclc[1]))) %>% 
      filter(
        (la_leche_count <= (input$filter_laleche[2])) & 
          (la_leche_count >= (input$filter_laleche[1]))) %>% 
      filter(
        (baby_friendly_count <= (input$filter_babyhospital[2])) &
          (baby_friendly_count >= (input$filter_babyhospital[1]))) %>% 
      filter(
        (wic_site_count <= (input$filter_wic[2])) &
          (wic_site_count >= (input$filter_wic[1]))) %>% 
      filter(
        (rucc <= (input$filter_rucc[2])) &
          (rucc >= (input$filter_rucc[1]))) %>% 
      filter(
        (svi <= (input$filter_svi[2])) &
          (svi >= (input$filter_svi[1]))) %>%
        select(as.vector(input$select_var))

    }) 
    # print table
    output$dt_table <- renderDataTable({
      get_dat <- get_dat()
      datatable(get_dat, options = list(scrollX = T))
    })
    #download
    output$dt_download<- downloadHandler(
      filename = "breastfeeding.csv",
      content = function(file) {
        write.csv(get_dat(), file, row.names = FALSE)
      }
    ) # end download
    
    
    output$explore_plot <- renderPlot({
      get_dat <- get_dat()
      if(input$plots == "Density"){
        ggplot(get_dat, 
               aes_string(x = input$densityx)) + 
          geom_density(position = "identity", alpha = 0.3) 
      }
      else if(input$plots == "Bar"){
        ggplot(get_dat, aes_string(x = input$barx)) + 
          geom_bar(aes(), position = "dodge") 
      }
      
    }) #end explore plot
})
