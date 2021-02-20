# ==============
# Making Health Economic Models Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield & Dark Peak Analytics
# contact: info@darkpeakanalytics.com
# ==============

# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

library(shiny)   # we need the function shiny installed, this loads it from the library.      
library(shinythemes)

## EXERCISE: adapt the script to run the Markov model and plot the results

## a) source the scripts that contain the model functions from the Markov Model/src folder.
  # source("Markov Model/src/f_gen_psa.R")
  # source("Markov Model/src/f_MM_sicksicker.R")
  # source("Markov Model/src/f_wrapper.R")

## NOTE: you can no longer use the button on the top-right to start the app
## you have to use shinyApp(ui, server), i.e. select and execute all code (Ctrl+A/CMD+A) 

## b) change the title of the app to 'Sicker sicker Model in Shiny'

## c) change the labels of the inputs to 'Treatment Costs', 'PSA runs' and 'Initial age'

## d) instead of using fun_shiny(...) to create num, use f_wrapper 

## e) instead of x and y, plot the output from f_wrapper:
##    i.e. plot the incremental costs against the incremental QALYs
##    set limits for the x-axis: 0 - 10000; and for the y-axis: 0 - 1.5


#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluidpage function
  theme = shinytheme("sandstone"),    # set a theme for your app
  # see https://rstudio.github.io/shinythemes/ for more options                       
  titlePanel("More complex model"),   # title of app
  
  
  
  # SIDEBAR
  sidebarLayout(    # indicates layout is going to be a sidebar-layout
    
    sidebarPanel( # open sidebar panel
      
      numericInput(inputId = "x",           # id of input, used in server
                   label = "Number X",          # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 1000),              # maximum value allowed
      
      numericInput(inputId = "y",      # id of input, used in server
                   label = "Number Y",        # label next to numeric input
                   value = 200,              # initial value
                   min = 0,                   # minimum value allowed
                   max = 400),                # maximum value allowed
      
      sliderInput(inputId = "z",  # id of input, used in server
                  label = "Number Z",      # label next to numeric input
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
                 
                 # HINT: use f_wrapper here and use x,y, and z to specify costs, psa runs, and age
                 
                 
                 #--- CREATE GRAPH IN SERVER ---#
                 output$graph <- renderPlot({
                    
                   plot(isolate(input$x), isolate(input$y))
                   
                   
                 }) # render Text end.
                 
               }) # Observe Event End  
  
} # Server end





## ----- run app------

shinyApp(ui, server)
