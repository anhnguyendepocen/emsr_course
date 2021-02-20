# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============

rm(list = ls())

# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

# we need the function shiny installed, this loads it from the library.
library(shiny)             

# a) Customize your app and include a plot. For example you may want to add:
#   A title to the app.
#   Add an explanation of the equations used in the function.
#   Add text to the sidebar panel.
#   To change the theme.

# b) Add a scatter plot with the x input on the X axis and the y input on the Y axis. 
# To do this you will need to use plotOutput() in the user interface function and 
# renderPlot in the server function. (Hint, to create a simple scatter plot, use the plot() function, e.g. plot(x,y)

#================================================================
#                   Create User Interface
#================================================================

ui <- fluidPage(    # create user interface using fluidpage function

  titlePanel("More complex model"),   # title of app
  

  
  # SIDEBAR
  sidebarLayout(    # indicates layout is going to be a sidebar-layout
    
    sidebarPanel( # open sidebar panel
      
      numericInput(inputId = "x",           # id of input, used in server
                   label = "Number X",          # label next to numeric input
                   value = 200,               # initial value
                   min = 0,                   # minimum value allowed
                   max = 100000),              # maximum value allowed
                   
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
  

  fun_shiny <- function(x,y,z){
    
    max_minus_mean = max(c(x,y,z)) - mean(c(x,y,z))
    
    max_minus_mean
    
  }
  
  
  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = F, {
                 
                 num = fun_shiny(x = input$x,
                                 y = input$y,
                                 z = input$z)
                 

  #--- CREATE GRAPH IN SERVER ---#
                 output$graph <- renderPlot({
                   
                   plot(input$x, input$y)
    
     }) # render Text end.
  
  }) # Observe Event End  

} # Server end





## ----- run app------

shinyApp(ui, server)
