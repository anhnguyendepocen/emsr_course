# ==============
# Making Health Economic Models Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield & Dark Peak Analytics
# contact: info@darkpeakanalytics.com
# ==============

# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

library(shiny)   # we need the function shiny installed, this loads it from the library.      

# 1. source the scripts that contain the model functions from the Markov Model/src folder.
# HINT: the function that is used to generate PSA inputs is hashed out as an example below.
# source("Markov Model/src/f_gen_psa.R")


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
                   max = 2000),                # maximum value allowed
      
      sliderInput(inputId = "SI_n_age_init",  # id of input, used in server
                  label = "Initial Age",      # label next to numeric input
                  value = 25,                 # initial value
                  min = 10,                   # minimum value allowed
                  max = 80),                  # maximum value allowed
      
      
      actionButton(inputId = "run_model",     # id of action button, used in server
                   label   = "Run model")     # action button label (on button)
      
    ),  # close sidebarPanel
    
    mainPanel(                                # open main panel
      
      # 2. INSERT THE TEXT OUTPUT HERE
      # HINT:  the output Id must match the name of the renderText object
      
                    
      
    ) # close mainpanel    
    
  ) # close sidebarlayout
  
) # close UI fluidpage


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output){   # server = function with two inputs
  
  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = F, {
                 
                 # 3. Run model wrapper function (from within the run_model observe event function) with the Shiny inputs 
                 #    and store the results as data-frame 
                
                        # < insert code here >
                 
                 
                 
                 # 4. calculate the ICER from the PSA outputs.
                 
                        # < insert code here >
                 
                 
                 # 5. Display the ICER using the renderText function
                 output$printICER <- renderText({
                   
                        # < insert code here >
                   
                 
                   }) # render Text end.
                 
               }) # Observe Event End
  
  
} # Server end



## ----- run app------

shinyApp(ui, server)