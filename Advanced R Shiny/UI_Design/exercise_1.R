# ==============
# Making Health Economic Models Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield & Dark Peak Analytics
# contact: info@darkpeakanalytics.com
# ==============

# Exercise: 
#            1. Use the CEP and CEAC functions we created in the previous session to make fancy plots in this app.

#            Extension: Put these plots in tabs so the app looks tidier. (https://shiny.rstudio.com/articles/tabsets.html)
#
#                       Change the option to select PSA runs to be a dropdown (selectInput). HINT: this input defaults to character, so you'll need to update to numeric.
#                       
#                       Change the runModel button so that it says the number of simulations that will be run 
#                       This link will help(https://mastering-shiny.org/action-dynamic.html#simple-uses)
                     


# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

# we need the function shiny installed, this loads it from the library.
library(shiny) 
library(shinyFeedback)
library(ggplot2)

# source the wrapper function.
source("Markov Model/src/f_gen_psa.R")
source("Markov Model/src/f_MM_sicksicker.R")
#source("Markov Model/src/f_wrapper.R")
source("Advanced R Shiny/UI_Design/f_wrapper_progress.R")
source("Advanced R Shiny/HE_plots/plotCEAC.R")
source("Advanced R Shiny/HE_plots/plotCEP.R")



#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluid-page function
  
  titlePanel("Sick Sicker Model in Shiny"),   # title of app
  
  # SIDEBAR
  sidebarLayout(    # indicates layout is going to be a sidebar-layout
    
    sidebarPanel( # open sidebar panel
      
      numericInput(inputId = "SI_c_Trt",      # id of input, used in server
                   label = "Treatment Cost",  # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 400),                # maximum value allowed
      
      numericInput(inputId = "SI_n_sim",      # id of input, used in server
                   label = "PSA runs",        # label next to numeric input
                   value = 1000,              # initial value
                   min = 0,                   # minimum value allowed
                   max = 2000),               # maximum value allowed
      
      sliderInput(inputId = "SI_n_age_range", # id of input, used in server
                  label = "Treatment Age",    # label next to numeric input
                  value = c(25, 75),          # initial value
                  min = 10,                   # minimum value allowed
                  max = 80),                  # maximum value allowed
      
      
      actionButton(inputId = "run_model",     # id of action button, used in server
                   label   = "Run model")     # action button label (on button)
      
    ),  # close sidebarPanel
    
    mainPanel(                                # open main panel
      
      h3("Results Table"),                    # heading (results table)                
      
      tableOutput(outputId = "SO_icer_table"),   # tableOutput id = icer_table, from server
      
      h3("Cost-effectiveness Plane"),         # heading (Cost effectiveness plane)
      
      plotOutput(outputId = "SO_CE_plane"),      # plotOutput id = CE_plane, from server
      
      #< INSERT CEAC CURVE HERE >#
      
      
      
    ) # close mainpanel    
    
  ) # close sidebarlayout
  
) # close UI fluidpage


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output){   # server = function with two inputs
  
  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = F, {
                 
                 tooBig <- input$SI_n_sim > 10000
                 
                 if(tooBig){
                 showModal(
                   
                   modalDialog(size = "s",
                               title = "This could take a while, don't hold your breath",
                               easyClose = T))
                 }
                 
                 # Run model wrapper function with the Shiny inputs and store as data-frame 
                 df_model_res = f_wrapper(c_Trt      = input$SI_c_Trt,
                                          n_age_init = input$SI_n_age_range[1],
                                          n_age_max  = input$SI_n_age_range[2],
                                          n_sim      = input$SI_n_sim)
                 
                 
                 #--- CREATE COST EFFECTIVENESS PLANE ---#
                 output$SO_icer_table <- renderTable({ # this continuously updates table
                   
                   df_res_table <- data.frame( # create dataframe
                     
                     Option =  c("Treatment","No Treatment"), 
                     
                     QALYs  =  c(mean(df_model_res$QALY_Trt),mean(df_model_res$QALY_NoTrt)),
                     
                     Costs  =  c(mean(df_model_res$Cost_Trt),mean(df_model_res$Cost_NoTrt)),
                     
                     Inc.QALYs = c(mean(df_model_res$QALY_Trt) - mean(df_model_res$QALY_NoTrt),NA),
                     
                     Inc.Costs = c(mean(df_model_res$Cost_Trt) - mean(df_model_res$Cost_NoTrt),NA)#,
                     
                     #ICER = c(mean(df_model_res$ICER),NA)
                   )
                   
                   df_res_table$ICER = df_res_table$Inc.Costs  /  df_res_table$Inc.QALYs
                   
                   # round the dataframe to two digits so looks tidier
                   df_res_table[,2:6] <- round(df_res_table[,2:6], digits = 2) 
                   
                   #print the dataframe
                   df_res_table
                   
                 }) # table plot end.
                 
                 
                 #---  CREATE COST EFFECTIVENESS PLANE ---#
                 output$SO_CE_plane <- renderPlot({ # render plot repeatedly updates.
                   
                   #< USE PLOTTING FUNCTION FOR CEP HERE >#

                 
                   }) # renderplot end
                 
                 
                 #---  CREATE CEAC ---#
                 output$SO_CEAC <- renderPlot({ # render plot repeatedly updates.
                   
                   #< USE PLOTTING FUNCTION FOR CEAC HERE >#

                 }) # renderplot end
                 
                 
               }) # Observe Event End
  
  
} # Server end





## ----- run app------

shinyApp(ui, server)