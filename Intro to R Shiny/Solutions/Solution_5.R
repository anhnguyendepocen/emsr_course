# ==============
# Making Health Economic Models Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield & Dark Peak Analytics
# contact: info@darkpeakanalytics.com
# ==============

# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

library(shiny)   # we need the function shiny installed, this loads it from the library.      
library(shinythemes)

source("Markov Model/src/f_gen_psa.R")
source("Markov Model/src/f_MM_sicksicker.R")
source("Markov Model/src/f_wrapper.R")

#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluidpage function
  theme = shinytheme("sandstone"),    # set a theme for your app
  # see https://rstudio.github.io/shinythemes/ for more options                       
  titlePanel("Sicker sicker Model in Shiny"),   # title of app
  
  
  
  # SIDEBAR
  sidebarLayout(    # indicates layout is going to be a sidebar-layout
    
    sidebarPanel( # open sidebar panel
      
      numericInput(inputId = "x",           # id of input, used in server
                   label = "Treatment Costs", # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 100000),              # maximum value allowed
      
      numericInput(inputId = "y",      # id of input, used in server
                   label = "PSA runs",        # label next to numeric input
                   value = 200,              # initial value
                   min = 0,                   # minimum value allowed
                   max = 400),                # maximum value allowed
      
      sliderInput(inputId = "z",  # id of input, used in server
                  label = "Initial age",      # label next to numeric input
                  value = 25,                 # initial value
                  min = 10,                   # minimum value allowed
                  max = 80),                  # maximum value allowed
      
      
      actionButton(inputId = "run_model",     # id of action button, used in server
                   label   = "Run model")     # action button label (on button)
      
      
    ),  # close sidebarPanel
    
    mainPanel(            # open main panel
      
      h3("Results") , 
      
      plotOutput(outputId = "graph")
      
    ) # close mainpanel    
    
  ) # close sidebarlayout
  
) # close UI fluid page


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output){   # server = function with two inputs
  
  
  
  
  observeEvent(input$run_model,{       # when action button pressed ...
                 
                 model_res = f_wrapper(
                   c_Trt = input$x,
                   n_sim = input$y,
                   n_age_init = input$z
                   )
                 
                 #--- CREATE GRAPH IN SERVER ---#
                 output$graph <- renderPlot({
                   
                   plot(x = isolate(model_res$Cost_Trt-model_res$Cost_NoTrt), 
                        y = isolate(model_res$QALY_Trt-model_res$QALY_NoTrt))

                 }) # render Text end.
                 
               }) # Observe Event End  
  
} # Server end





## ----- run app------

shinyApp(ui, server)
