

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  


####################################################################


output$sale <- renderInfoBox({
  
  infoBox("NUMBER of OBSERVATION",nrow(df),
  icon = icon("list-alt"),
  color = "orange", fill=TRUE
  )
})


output$o <- renderInfoBox({
infoBox("NUMBER of VARIABLES", ncol(df),  
  icon = icon("table"),
  color = "green", fill=TRUE
  )
})


output$product <- renderInfoBox({
infoBox("NUMBER of MISSING VALUES",  sum(is.na(df)),  
  icon = icon("info"),
  color = "red", fill=TRUE
  )
})

output$table<- DT::renderDataTable({
  #df <- df_products_upload()
  DT::datatable( df, options = list(pageLength = 10,scrollX=TRUE) )
})


output$summaryTable1 <- renderUI({
  
  
  out <- print(dfSummary(df, 
                         graph.magnif = 0.7),
               method = 'render',
               omit.headings = TRUE,
               bootstrap.css = FALSE,
               graph.col = st_options("dfSummary.graph.col"),
               graph.magnif = st_options("dfSummary.graph.magnif"))
  
  return(out)

})


output$Yplot <- renderPlot({
  
  if (class(df[[input$xvar]]) == "integer" ||
      class(df[[input$xvar]]) == "numeric") {
    
    explore_distribution_numeric(df[[input$xvar]],vname = input$xvar)
    
    
  } else {
    distobj <- explore_distribution_categorical(df[[input$xvar]],vname = input$xvar)
    distobj$distplot
    
    
  }
  
  
})

callModule(module = esquisserServer, id = "esquisse")
#############################################
######################################MODEL################################################

###Data prepare
htoo<-reactive({
  
  
  #string_2_factor_names <- df %>%
    #select_if(is.character) %>%
   # names()
  
  unique_numeric_values_tbl <-df  %>%
    select_if(is.numeric) %>%
    map_df(~ unique(.) %>% length()) %>%
    gather() %>%
    arrange(value) %>%
    mutate(key = as_factor(key))
  
 # factor_limit <- 7
  
  #num_2_factor_names <- unique_numeric_values_tbl %>%
   # filter(value < factor_limit) %>%
  #  arrange(desc(value)) %>%
   # pull(key) %>%
    #as.character()
  
 # rec_obj <- recipe(~ ., data = df) %>%
    #step_string2factor(string_2_factor_names) %>%
   # step_num2factor(num_2_factor_names) %>%
   # prep(stringsAsFactors = FALSE)
 # train_new <- bake(rec_obj, df)
  data_h2o <- as.h2o(df)
  
  return(data_h2o)
  
})

#observe({
#nms2 <- names(htoo())
#updateSelectizeInput(session,'ytarget', choices=nms2,selected=nms2[1])
#updateSelectizeInput(session,'xfeatures', choices=nms2, selected=nms2)
#})  

split<-eventReactive(input$train,{
  splits_h2o <- h2o.splitFrame(htoo(), ratios = input$sld_testsplit, seed = 1358)
  return(splits_h2o)
})

aml <- eventReactive(input$train,{
  #train_h2o <- splits_h2o[[1]]
  #test_h2o <- splits_h2o[[2]]
  
  y <- input$ytarget
  x <- setdiff(input$xfeatures, y)
  
  aml <- h2o.glm(family= "gaussian", x= x, y=y, training_frame= split()[[1]],  lambda = 0, 
                 compute_p_values = TRUE, max_runtime_secs = 30, remove_collinear_columns = TRUE)
  return(aml)
})  

#observeEvent(input$button,{


output$my<- renderTable({
  
  lb <- aml()@model$coefficients_table
  
  models=as.data.frame(lb)
  
  
  return(models)
  
})

#output$confusion_matrix<- renderDataTable({
#  automl_leader <-aml()@leader
#  performance_h2o <- h2o.performance(automl_leader, newdata = split()[[2]])
  
#  confusion=performance_h2o %>%
 #   h2o.confusionMatrix()
  
# conmatrix=DT::datatable(as.data.frame(confusion))
 # return(conmatrix)
#})

output$rocplot <- renderPlot({
  h2o.std_coef_plot(aml())
})


#output$import <- renderPlot({
  
 # automl_leader2 <-aml()@leader
 # importance=h2o.varimp_plot(automl_leader2, num_of_features = 10 )
 # return(importance)
#})  
output$selected_var <- renderUI({ 
  h2o.performance(aml(), newdata =split()[[2]])
})
#########################Predictions##############################
test<-reactive({
  validate(
    need(input$clientID != "", "Please do not panic. You are getting this error becasue you did not select ClientID  and Actual Target columns yet. Please make sure to select Client Identification number first followed by Actual Target column !")
  )
  
  prediction_h2o <- h2o.predict(aml(), newdata = split()[[2]]) 
  pred=as.data.frame(prediction_h2o)
  
  
  ID=as.data.frame(split()[[2]][, input$clientID])
  
  d=bind_cols(ID,pred )
  return(d)
})


output$h2oprediction<- renderTable({
  test()
})


############################################
# Download prediction data.
output$downloadmodelpredict <- downloadHandler(
  filename = "predictions.csv",
  content = function(con) {
    write_csv(test(), con)
  }
)

##################################################
output$lime <- renderPlot({
  #Run lime() on training set
  explainer <- lime::lime(
    as.data.frame(split()[[1]]), 
    model= aml(), 
    bin_continuous = TRUE)
  
  
  #Run explain() on explainer
  explanation <- lime::explain(
    as.data.frame(split()[[2]][1:3]), 
    explainer    = explainer, 
    n_labels     = 1, 
    n_features   = 5,
    kernel_width = 0.5)
  
  plot_features(explanation) +
    labs(title = " Predictive Analytics: LIME Feature  Visualization",
         subtitle = "Hold Out (Test) Set, selected Cases Shown")
})



  
















  })




