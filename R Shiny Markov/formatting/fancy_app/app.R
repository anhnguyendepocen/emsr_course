
# ==============
# Making Markov Models Shiny 
# Robert Smith & Paul Schneider
# University of Sheffield
# contact: rasmith3@sheffield.ac.uk
# ==============

rm(list = ls())

# get the following functions from the library
# if you don't have them use install.packages("<package_name>")
library(shiny) 
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)

# source the components of the app- 
# note if you are not using GitHub you will need to change the paths.

# model wrapper function and plotting functions
source("course_content/session_4/formatting/fancy_app/wrapper.R") 
source("course_content/session_4/formatting/fancy_app/plotFunctions.R")

# source the user-interface components
source("course_content/session_4/formatting/fancy_app/header1.R")
source("course_content/session_4/formatting/fancy_app/sidebar1.R")
source("course_content/session_4/formatting/fancy_app/body1.R")
source("course_content/session_4/formatting/fancy_app/footer1.R")


# source the UI itself (it simply calls the above)
source("course_content/session_4/formatting/fancy_app/ui.R")

# source the server function
source("course_content/session_4/formatting/fancy_app/server.R")

# run the app
shinyApp(ui = ui, server = server)


