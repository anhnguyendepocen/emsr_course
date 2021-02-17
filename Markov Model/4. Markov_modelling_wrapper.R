# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============


#################### using wrapper function for R shiny #############

rm(list = ls())  # delete everything that is in R's memory

# source custom functions
source("Markov Model/src/f_gen_psa.R")
source("Markov Model/src/f_MM_sicksicker.R")
source("Markov Model/src/f_wrapper.R")

f_wrapper <- function(
  
  #-- User adjustable inputs --#
  
  # age at baseline
  n_age_init = 25, 
  # maximum age of follow up
  n_age_max  = 110,
  # discount rate for costs and QALYS 
  d_r = 0.035,
  # number of simulations
  n_sim   = 1000,
  # cost of treatment
  c_Trt   = 50       
  
){
  
  # need to specify environment of inner functions (to use outer function environment)
  # alternatively - define functions within the wrapper function.
 environment(f_gen_psa)         <- environment()
 environment(f_MM_sicksicker)   <- environment()
  
  #-- Nonadjustable inputs --#
  
  #  number of cycles
  n_t <- n_age_max - n_age_init
  # the 4 health states of the model:
  v_n <- c("H", "S1", "S2", "D") 
  # number of health states 
  n_states <- length(v_n) 
  
  #-- Create PSA Inputs --#
  
  df_psa <- f_gen_psa(n_sim = n_sim, 
                      c_Trt =  c_Trt)
  
  #--  Run PSA  --#
  
  # Initialize matrix of results outcomes
  m_out <- matrix(NaN, 
                  nrow = n_sim, 
                  ncol = 5,
                  dimnames = list(1:n_sim,
                                  c("Cost_NoTrt", "Cost_Trt",
                                    "QALY_NoTrt", "QALY_Trt",
                                    "ICER")))
  
  # run model for each row of PSA inputs
  for(i in 1:n_sim){
    
    # store results in row of results matrix
    m_out[i,] <- f_MM_sicksicker(df_psa[i, ])
    
  } # close model loop
  
  
  #-- Return results --#
  
  # convert matrix to dataframe (for plots)
  df_out <- as.data.frame(m_out) 
  
  # output the dataframe from the function  
  return(df_out) 
  
} # end of function


# The wrapper returns a data frame with the costs and the QALY results for Treatment and No Treatment
# and the corresponding ICER. Each row corresponds to one PSA-draw. To check what the result looks like,
# we can run the model, manually specifying the treatment costs (c_Trt=100), initial age (n_age_init = 25), 
# and number of draws (n_sim = 1000).

df_model_res = f_wrapper(
  c_Trt = 200,
  n_age_init = 25,
  n_sim = 1000)

head(df_model_res)

