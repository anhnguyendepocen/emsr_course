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


# plot cost-effectiveness plane
ggplot(results) +
  # reference lines (threshold at 20k)
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 20000, linetype = "dashed") +
  # PSA icers and mean icer
  geom_point(aes(x = QALY_Trt-QALY_NoTrt, y = Cost_Trt-Cost_NoTrt), col = "cadetblue", size =.7, alpha =.7) +
  geom_point(aes(x = mean(QALY_Trt-QALY_NoTrt), y = mean(Cost_Trt-Cost_NoTrt)), col = "blue") +
  # labels
  xlab("Incremental QALYs") +
  ylab("Incremental Costs") +
  theme_minimal()












