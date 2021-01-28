# ==============
# 
# SCRIPT 9 - HTML formatting
#
# Making Markov Models Shiny 
# Robert Smith & Paul Schneider
# University of Sheffield
# contact: rasmith3@sheffield.ac.uk
# ==============

rm(list = ls())
# install.packages("shiny") # necessary if you don't already have the function 'shiny' installed.

# we need the function shiny installed, this loads it from the library.
library(shiny) 
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(shinydashboard)

# source the wrapper function - note if you are not using 
# GitHub you will need to change the path to the location of wrapper.R
source("./course_content/session_4/formatting/wrapper.R") 
source("./course_content/session_4/formatting/header1.R")
source("./course_content/session_4/formatting/sidebar1.R")
source("./course_content/session_4/formatting/body1.R")
source("./course_content/session_4/formatting/plotFunctions.R")


#================================================================
#                   Create User Interface
#================================================================

ui <- dashboardPage(
  
    header = header1,
  
    sidebar = sidebar1,
  
    body = body1,
  
    skin = "blue",
    
    title = "Sick Sicker Model in Shiny"
  
)
    


#================================================================
#                     Create Server Function
#================================================================

server <- function(input, output, session){   # server = function with two inputs
  
  # reactive function to get the csv file
  getCSV_uTrt <- reactive({
    
    file <- input$file1
    
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    validate(need(ext == "csv", "Please upload a csv file"))
    
    CSV_u_Trt <- read.csv(file$datapath, header = input$header,row.names = 1)
    
    CSV_u_Trt$u_Trt
    
  })
  
  # load the file and show it in the Contents tab
  output$contents <- renderTable({
    data = getCSV_uTrt()
    data.frame(mean = mean(data),
               sd = sd(data),
               class = class(data),
               min = min(data),
               max = max(data),
               N_obs = length(data),
               NAs = sum(is.na(data)))
    
  })
  
  # update the run_model button to show number of PSAs
  observeEvent(input$SI_n_sim, {
    label <- paste0("Run ", input$SI_n_sim, " PSA runs")
    updateActionButton(session, "run_model", label = label)
  })
  
  

  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = F, {
                 
                 
                   if(input$use_CSV){
                     print("using CSV")
                     temp_U_Trt <- sample(getCSV_uTrt(),size = 1000,replace = T)
                   } else {
                     print("not using CSV")
                     temp_U_Trt <- rtruncnorm(1000, mean = 0.95, sd = 0.02, b = 1)
                   }
                 
                # Run model wrapper function with the Shiny inputs and store as data-frame 
                 df_model_res = f_wrapper(c_Trt = input$SI_c_Trt,
                                          n_age_init = input$SI_n_age_init,
                                          n_sim = input$SI_n_sim,
                                          u_Trt = temp_U_Trt
                                          )
                 
                 
                 #--- CREATE COST EFFECTIVENESS PLANE ---#
                 output$SO_icer_table <- renderTable({ # this continuously updates table
                   
                   df_res_table <- data.frame( # create data-frame
                     
                     Option =  c("Treatment","No Treatment"), 
                     
                     QALYs  =  c(mean(df_model_res$QALY_Trt),mean(df_model_res$QALY_NoTrt)),
                     
                     Costs  =  c(mean(df_model_res$Cost_Trt),mean(df_model_res$Cost_NoTrt)),
                     
                     Inc.QALYs = c(mean(df_model_res$QALY_Trt) - mean(df_model_res$QALY_NoTrt),NA),
                     
                     Inc.Costs = c(mean(df_model_res$Cost_Trt) - mean(df_model_res$Cost_NoTrt),NA),
                     
                     ICER = c(mean(df_model_res$ICER),NA)
                   )
                   
                   # round the dataframe to two digits so looks tidier
                   df_res_table[,2:6] <- round(df_res_table[,2:6],digits = 2) 
                   
                   #print the dataframe
                   df_res_table
                   
                 }) # table plot end.
                 
                 
                 #---  CREATE COST EFFECTIVENESS PLANE ---#
                 output$SO_CE_plane <- renderPlot({ # render plot repeatedly updates.
                   
                   # use function ce_plot from above file to create plot.
                   plot <- ce_plot(results = df_model_res)
                   
                   # save cost-effectiveness plane for download
                   ceP_download <<-  reactive({plot})
                   
                   # output plot from function.
                   plot
                   
                 }) # render plot end
                 
                 
                 # cost effectiveness plane fig. download ----
                 output$cep = downloadHandler(
                   filename = 'ce_plane.png',    # select file name
                   content = function(file) {
                     device <- function(..., width, height) {
                       grDevices::png(..., 
                                      width = width, 
                                      height = height,
                                      res = 300, 
                                      units = "in")
                     }
                     ggsave(file, 
                            plot = ceP_download(), # need to remember to have "()" after the ceP_download we created above!
                            device = device)
                   })
                 
                 # data downloader
                 output$downloadData <- downloadHandler(
                   filename = function() {
                     paste(input$dataset, ".csv", sep = "")
                   },
                   content = function(file) {
                     write.csv(df_model_res, file, row.names = FALSE)
                   }
                 )
                 
                 
               }) # Observe Event End
  
  

  

  
  
} # Server end





## ----- run app------

shinyApp(ui, server)

