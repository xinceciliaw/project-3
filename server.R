

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
    
    explore_data <- reactive({
      explore_data <- dat %>% 
        filter(
          (bf_pct <= (input$explore_filtervar_bi[2])) &
            (bf_pct >= (input$explore_filtervar_bi[1]))) %>% 
        filter(
          (im_rate <= (input$explore_filtervar_im[2])) &
            (im_rate >= (input$explore_filtervar_im[1]))) %>% 
        filter(
          (ibclc_rate <= (input$explore_filtervar_ibclc[2])) &
            (ibclc_rate >= (input$explore_filtervar_ibclc[1]))) %>%
        filter(
          (la_leche_count <= (input$explore_filtervar_laleche[2])) &
            (la_leche_count >= (input$explore_filtervar_laleche[1]))) %>%
        filter(
          (baby_friendly_count <= (input$explore_filtervar_babyhospitcal[2])) &
            (baby_friendly_count >= (input$explore_filtervar_babyhospitcal[1]))) %>%
        filter(
          (wic_site_count <= (input$explore_filtervar_wic[2])) &
            (wic_site_count >= (input$explore_filtervar_wic[1]))) %>%
        filter(
          (rucc <= (input$explore_filtervar_rucc[2])) &
            (rucc >= (input$explore_filtervar_rucc[1]))) %>%
        filter(
          (svi <= (input$explore_filtervar_svi[2])) &
            (svi >= (input$explore_filtervar_svi[1])))
    })
    
    
    output$explore_plot <- renderPlotly({
      explore_data <- explore_data()
      if(input$plots == "Density"){
        ggplot(explore_data, 
               aes_string(x = input$densityx)) + 
          geom_density(position = "identity", alpha = 0.3) 
      }
      else if(input$plots == "Bar"){
        ggplot(explore_data, aes_string(x = input$barx)) + 
          geom_bar(aes(), position = "dodge") 
      }
      
    }) #end explore plot
    
    numerical_summary_output <- reactive({
      explore_data <- explore_data()
      if(input$numsumary == "Summary Statistics"){
        explore_summary_variable <- explore_data %>% select(input$fivenum) %>% pull()
        explore_summary_output <- c(summary(explore_summary_variable), 
                                    "St.Dev."=sd(explore_summary_variable)) %>% 
          t() %>% as.data.frame(row.names = as.character(input$fivenum)) %>% round(4)
      }
      else if (input$numsumary == "Correlation Table"){
        explore_summary_output <- explore_data %>% select(input$Correlation) %>% 
          cor(method = "pearson") %>% round(4)
      }
      return(explore_summary_output)
    })
    output$explore_numerical_summary <- renderDataTable({
      numerical_summary_output <- numerical_summary_output()
      datatable(numerical_summary_output, options = list(scrollX = T))
    }) #end tables
    
    #start training and test
    train_test_data <- reactive({
      set.seed(1)
      train <- sample(1:nrow(dat), size = nrow(dat)*(as.numeric(input$trainset)/100))
      test <- dplyr::setdiff(1:nrow(dat), train)
      training_data <- dat[train, ]
      test_data <- dat[test, ]
      
      return(list("training_data"=training_data,"test_data"=test_data))
      
    }) #end training & test data set
    
    # update mtry
    observe({
      updateSliderInput(session, inputId = "mtry", value = length(input$select_predictor))
    })
    
    #tuning 
    modeling_parameters <- reactive({
      
      # create formula for modeling
      if(input$model_mlr == 1 ) {
        predictors <- paste(input$select_predictor, collapse = "+")
      } 
      response <- paste("bf_pct")
      formula <- as.formula(paste(response,"~",predictors))
      
      # cv
      trControl <-  trainControl(method = "cv", number = input$folds)
      
      # tuning grid
      tree_grid <- data.frame(cp = 0.001:(input$cp))
      rf_grid <- data.frame(mtry = 1:(input$mtry))
      
      return(list("formula"=formula, "trControl"=trControl, "tree_grid"=tree_grid, "rf_grid"=rf_grid))
      
    })
    
    # multiple linear regression
    
    # model multiple linear regression
    fit_mlr <- eventReactive(input$submit_models, {
      modeling_parameters <- modeling_parameters()
      train_test_data <- train_test_data()
      if(input$model_mlr==1) {
        fit_mlr_model <- train(modeling_parameters[["formula"]],
                               data = train_test_data[["training_data"]],
                               method = "lm",
                               preProcess = c("center", "scale"),
                               trControl = modeling_parameters[["trControl"]])
        predict_mlr <- postResample(predict(fit_mlr_model, newdata = train_test_data[["test_data"]]), 
                                    obs = train_test_data[["test_data"]]$bf_pct)
        sum_mlr <- summary(fit_mlr_model)
        return(list("fit_mlr_train"=fit_mlr_model, "fit_mlr_test"=predict_mlr, "fit_sum_mlr" = sum_mlr ))
      }  
    })
    
    rmse_training_mlr <- reactive({
      fit_mlr <- fit_mlr()
      fit_mlr_rmse <- fit_mlr[["fit_mlr_train"]]$results["RMSE"] %>% min() %>% as.data.frame()
      colnames(fit_mlr_rmse) <- "RMSE"
      rownames(fit_mlr_rmse) <- "Multiple Linear Regression"
      fit_mlr_rmse
    })
    
    output$rmse_training_mlr <- renderPrint({
      rmse_training_mlr <- rmse_training_mlr()
      rmse_training_mlr
    })
    
    output$result_training_mlr <- renderPrint({
      fit_mlr <- fit_mlr()
      fit_mlr[["fit_sum_mlr"]]
    })
    
    rmse_testing_mlr <- reactive({
      fit_mlr <- fit_mlr()
      mlr_test_rmse <- fit_mlr[["fit_mlr_test"]]["RMSE"] %>% min() %>% as.data.frame()
      colnames(mlr_test_rmse) <- "RMSE"
      rownames(mlr_test_rmse) <- "Multiple Linear Regression"
      mlr_test_rmse
    })
    
    output$rmse_testing_mlr <- renderPrint({
      rmse_testing_mlr <- rmse_testing_mlr()
      rmse_testing_mlr
    })
    
    
    # regression tree
    # model regression tree
    fit_tree <- eventReactive(input$submit_models, {
      modeling_parameters <- modeling_parameters()
      train_test_data <- train_test_data()
      if(input$model_regtree==1) {
        fit_tree_model <- train(modeling_parameters[["formula"]],
                                data = train_test_data[["training_data"]],
                                method = "rpart",
                                preProcess = c("center", "scale"),
                                trControl = modeling_parameters[["trControl"]],
                                tuneGrid = modeling_parameters[["tree_grid"]])
        predict_tree <- postResample(predict(fit_tree_model, newdata = train_test_data[["test_data"]]), 
                                     obs = train_test_data[["test_data"]]$bf_pct)
        sum_tree <- summary(fit_tree_model)
        return(list("fit_tree_train"=fit_tree_model, "fit_tree_test"=predict_tree, "fit_sum_tree" = sum_tree ))
      }  
    })
    
    rmse_training_tree <- reactive({
      fit_tree <- fit_tree()
      fit_tree[["fit_tree_train"]]
      fit_tree_rmse <- fit_tree[["fit_tree_train"]]$results["RMSE"] %>% min() %>% as.data.frame()
      colnames(fit_tree_rmse) <- "RMSE"
      rownames(fit_tree_rmse) <- "Regression Tree"
      fit_tree_rmse
    })
    
    output$rmse_training_tree <- renderPrint({
      rmse_training_tree <- rmse_training_tree()
      rmse_training_tree
    })
    
    output$result_training_tree <- renderPrint({
      fit_tree <- fit_tree()
      fit_tree[["fit_sum_tree"]]
    })
    
    rmse_testing_tree <- reactive({
      fit_tree <- fit_tree()
      tree_test_rmse <- fit_tree[["fit_tree_test"]]["RMSE"] %>% min() %>% as.data.frame()
      colnames(tree_test_rmse) <- "RMSE"
      rownames(tree_test_rmse) <- "Regression Tree"
      tree_test_rmse
    })
    
    output$rmse_testing_tree <- renderPrint({
      rmse_testing_tree <- rmse_testing_tree()
      rmse_testing_tree
    })
    
    
    # random forest
    # model random forest
    fit_rf <- eventReactive(input$submit_models, {
      modeling_parameters <- modeling_parameters()
      train_test_data <- train_test_data()
      if(input$model_rf==1) {
        fit_rf_model <- train(modeling_parameters[["formula"]],
                              data = train_test_data[["training_data"]],
                              method = "rf",
                              preProcess = c("center", "scale"),
                              trControl = modeling_parameters[["trControl"]],
                              tuneGrid = modeling_parameters[["rf_grid"]])
        predict_rf <- postResample(predict(fit_rf_model, newdata = train_test_data[["test_data"]]), obs = train_test_data[["test_data"]]$bf_pct)
        sum_rf <- summary(fit_rf_model)
        return(list("fit_rf_train"=fit_rf_model, "fit_rf_test"=predict_rf, "fit_sum_rf" = sum_rf ))
      }  
    })
    
    rmse_training_rf <- reactive({
      fit_rf <- fit_rf()
      fit_rf[["fit_rf_train"]]
      fit_rf_rmse <- fit_rf[["fit_rf_train"]]$results["RMSE"] %>% min() %>% as.data.frame()
      colnames(fit_rf_rmse) <- "RMSE"
      rownames(fit_rf_rmse) <- "Random Forest"
      fit_rf_rmse
    })
    
    output$rmse_training_rf <- renderPrint({
      rmse_training_rf <- rmse_training_rf()
      rmse_training_rf
    })
    
    output$result_training_rf <- renderPrint({
      fit_rf <- fit_rf()
      fit_rf[["fit_sum_rf"]]
    })  
    
    rmse_testing_rf <- reactive({
      fit_rf <- fit_rf()
      rf_test_rmse <- fit_rf[["fit_rf_test"]]["RMSE"] %>% min() %>% as.data.frame()
      colnames(rf_test_rmse) <- "RMSE"
      rownames(rf_test_rmse) <- "Random Forest"
      rf_test_rmse
    })  
    
    output$rmse_testing_rf <- renderPrint({
      rmse_testing_rf <- rmse_testing_rf()
      rmse_testing_rf
    })  
    
    
    # training rmse info for all models
    output$rmse_training_all_model <- renderTable({
      rmse_training_mlr <- rmse_training_mlr()
      rmse_training_tree <- rmse_training_tree()
      rmse_training_rf <- rmse_training_rf()
      table <- rbind(rmse_training_mlr, rmse_training_tree, rmse_training_rf)
      table$Model <- c("Multiple Linear Regression", "Regression Tree", "Random Forest")
      table %>% select(Model, RMSE)
    })
    
    # test rmse info for all models
    output$rmse_test_all_model <- renderTable({
      rmse_testing_mlr <- rmse_testing_mlr()
      rmse_testing_tree <- rmse_testing_tree()
      rmse_testing_rf <- rmse_testing_rf()
      table <- rbind(rmse_testing_mlr, rmse_testing_tree, rmse_testing_rf)
      table$Model <- c("Multiple Linear Regression", "Regression Tree", "Random Forest")
      table %>% select(Model, RMSE)
    })
    
    #importance plot
    rf_plot <- eventReactive(input$submit_models, {
      train_test_data <- train_test_data()
      predictors <- paste(input$select_predictor, collapse = "+")
      response <- paste("bf_pct")
      formula <- as.formula(paste(response,"~",predictors))
      Variance.Importance.Dotchart <- randomForest(formula, 
                                                   data = train_test_data[["training_data"]], 
                                                   mtry = 1:(input$mtry),
                                                   importance = TRUE)
      varImpPlot(Variance.Importance.Dotchart)
    })
    
    output$summary_rf<- renderPlot({
      rf_plot <- rf_plot()
    })
    
    # prediction
    prediction_result <- eventReactive(input$submit_predict, {
      ibclc_rate <- input$predict_ibclc
      la_leche_count <- input$predict_la_leche
      baby_friendly_count <- input$predict_babyhospital
      wic_site_count <- input$predict_wic
      rucc <- input$predict_rucc
      svi <- input$predict_svi
 
      predict_data <- as.data.frame(cbind(ibclc_rate,
                                          la_leche_count,
                                          baby_friendly_count,
                                          wic_site_count,
                                          rucc,
                                          svi))
      fit_mlr <- fit_mlr()
      fit_tree <- fit_tree()
      fit_rf <- fit_rf()
      if(input$predict_select_model=="Multiple Linear Regression"){
        predict_result <- predict(fit_mlr[["fit_mlr_train"]],
                                  newdata = predict_data) %>% round(3)
      } else if(input$predict_select_model=="Regression Tree") {
        predict_result <- predict(fit_tree[["fit_tree_train"]],
                                  newdata = predict_data) %>% round(3)
      } else if(input$predict_select_model=="Random Forest") {
        predict_result <- predict(fit_rf[["fit_rf_train"]],
                                  newdata = predict_data) %>% round(3)
      }
      return(list("Model Prediction" = predict_result))
      
    })
    
    output$predict_value_table <- renderTable({
      prediction_result <- prediction_result()
      print(prediction_result)
    })
    
      
})



