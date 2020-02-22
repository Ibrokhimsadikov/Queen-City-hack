#   ____________________________________________________________________________
#   UI                                                                      ####




### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage(id='AC', title ='MaYa', 
                   theme = "style/style.css",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
 
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home", icon = icon("home"),
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Dashboard
                   tabPanel("Data", icon = icon("database"),
                            useShinydashboard(),
                             # include dependencies
                            fluidRow(infoBoxOutput("sale"),
                                     valueBoxOutput("o"),
                                     valueBoxOutput("product")),                   
                            
      
                              
      
                     
                              
                          
      
   DT::dataTableOutput("table")
      
      
                            
      
                            
                                      
                     
                            
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - Model
                   tabPanel("EDA", #useShinydashboard(), 
                            icon = icon('chart-bar'), #'cogs'
                            
                             
                              
                              tabBox(
                                 width='1000',  title = tagList(shiny::icon("bar-chart-o"), "Exploratory Data Analysis "),  
                                selected = "Summary Stats", 
                                tabPanel("Summary Stats", uiOutput("summaryTable1")),
                                tabPanel("Variable Distributions", selectInput('xvar',label=label.help('Choose variable:','lbl_xvar'), choices=coln, multiple=FALSE),
                                         bsTooltip(id = "lbl_xvar", title = "Try and see distribution plot of selected variable", 
                                                   placement = "right", trigger = "hover"),
                                         hr(),
                                         plotOutput('Yplot',height=400)),
                                tabPanel("AutoPlot", esquisserUI(
                                  id = "esquisse", 
                                  header = FALSE, # dont display gadget title
                                  choose_data = FALSE, # dont display button to change data,
                                  container = esquisseContainer(height = "700px")
                                                                )  
                                         )
                                    )
                            
                                
                              
                              
                            
                                    
                                    
                                
                                    
                                    
                                       
                                    
                                 
                    
                            
                           
                      
                            
                            ),
    
     
                   # ----------------------------------
    tabPanel('Model', icon = icon('cogs'),
             
             tabItem("modelpara",   
                     box(
                       title = "Model Attributes", width=3, status = 'primary', solidHeader = TRUE,
                       p('Please be patient while model is being trained, it will take awhile once run button below pressed.'),
                       p( 'Also keep in mind as it is Credit Risk Modelling platform,  Confusion matrix, ROC curve and Variable importance 
                          plot only work if leader model has those parameters. In other words our modelling mainly designed for classification tasks'),
                       br(),
                       selectizeInput('ytarget',label='Select class variable', choices=coln, multiple=FALSE),
                       selectizeInput('xfeatures',label='Select independant variables',choices=coln,  multiple = T),
                       hr(),
                       
                       sliderInput('sld_testsplit',label = label.help('Set Train fraction','lbl_testsplit'),min = 0.5,max = 0.9, value = 0.7),
                       bsTooltip(id = "lbl_testsplit", title = "Select fraction of data to set aside for train data", 
                                 placement = "right", trigger = "hover"),
                       actionButton( 'train', label = 'Click to Run AutoML',icon = icon('cogs'), class='btn-danger fa-lg'
                       ),
                       hr(),
                       helpText('Once you are satisfied with model output you can generate predictions on Test data'),
                       actionButton('btn_viewPred',label = 'View AutoMl Predictions',icon=icon('table'),class='btn-success fa-lg')
                       
                       
                       ),
                     bsModal('prediction',title = 'H2O Leader Prediction',trigger = 'btn_viewPred',size = 'large',
                             selectizeInput('clientID',label='Select first clientID column and then Actual Target Column', choices=coln,  multiple=T),
                             h4('The table below is the Predictions of AutoML Leader Model on Test Data') ,
                             p('So here you can compare model predictions with Actual Target and CustomerID it belongs too'), 
                             div(style='max-height:300px; overflow-y: scroll; position: relative',withSpinner(tableOutput('h2oprediction'))),
                             hr(),
                             downloadButton("downloadmodelpredict", "Download simulated data for trial")
                             ),
                     
                     
                     
                     
                     box(title = "Model Outputs", width=9, status = 'primary', solidHeader = TRUE,
                         h4('The table below is the Outcome of H2O AutoML') ,
                         withSpinner(tableOutput('my')),
                         hr(),
                         
                         DT::dataTableOutput('confusion_matrix'),
                         hr(),
                         h4('The plot below is the Outcome of H2O Leader Magnituted of Coef.'),
                         plotOutput('rocplot',height=350),
                         hr(),
                         h4('The plot below is the Outcome of H2O Leader Test Data Performance '),
                         plotOutput('import',height=350),
                         uiOutput("selected_var")
                         
                     ),
                     box(title = "Model Local Interpretations: LIME", width=12, status = 'primary', solidHeader = TRUE,
                         selectInput("dept","SELECT CLIENT ID:", character(0), multiple = FALSE),
                         plotOutput('lime')
                         ))
             
             
             
             
    )
            
                     
                          
                          
                   
              
              
    
   
          
            
            
            
            
           
            
                   
            
              
              
        
             
                    
     
))



