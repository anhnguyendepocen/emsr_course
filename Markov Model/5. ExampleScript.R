# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============

# clear global environment
rm(list =ls())

# load necessary packages
library(ggplot2)

# source custom functions
source("Markov Model/src/f_gen_psa.R")
source("Markov Model/src/f_MM_sicksicker.R")
source("Markov Model/src/f_wrapper.R")

# run PSA - shoulc be very fast
tic <- Sys.time() # record start time
  
results <- f_wrapper(c_Trt = 2000,
                     n_age_init = 25,
                     n_age_max = 100,
                     d_r = 0.035,
                     n_sim = 1000)

toc <- Sys.time() # record end time

# print time difference
toc - tic












