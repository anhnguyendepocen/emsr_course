# ==============
# Making Health Economic Modelling Shiny
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield
# contact: info@darkpeakanalytics.com
# ==============

# clear global environment
rm(list =ls())

# USING ANOTHER PACKAGE - DAMPACK:
# https://rdrr.io/github/feralaes/dampack/man/ 
library(ggplot2)
#devtools::install_github("feralaes/dampack")
library(dampack)

# source custom functions
source("Markov Model/src/f_gen_psa.R")
source("Markov Model/src/f_MM_sicksicker.R")
source("Markov Model/src/f_wrapper.R")


# define parameters
n_iterations <- 10000
lambda_range = c(0, 100000)
v_wtp = seq(from = lambda_range[1], 
            to = lambda_range[2],
            length.out = 5000)
population <- 1000


# run the model
results <- f_wrapper(c_Trt = 970,
                     n_age_init = 25,
                     n_age_max = 100,
                     d_r = 0.07,
                     n_sim = n_iterations)

# see first 6 lines
head(results)

# use the evpi package, see documentation
?evpi

# create matrix of costs & qalys
m_qalys = as.matrix(results[, c("QALY_NoTrt", "QALY_Trt")]) 
m_costs = as.matrix(results[, c("Cost_NoTrt", "Cost_Trt")])

#m_qalys[, c("QALY_NoTrt", "QALY_Trt")] <- m_qalys[, c("QALY_NoTrt", "Cost_NoTrt"]

 # calculate the EVPI for range of lambda
l_evpi_results <- evpi(v.wtp = v_wtp,
                       m.e = m_qalys,
                       m.c = m_costs,
                       pop = population)

# create a very simple plot
plot(x = l_evpi_results$WTP,
     y = l_evpi_results$EVPI,
     type = "l",
     xlab = "Willingness to Pay per QALY (£)",
     ylab = "Population EVPI (£)")



# WITHOUT USING A PACKAGE
  
  
  # initialise data-frame with WTP
  df_evpi <- data.frame(WTP = v_wtp,
                        EVPI = NA)
  
  
  # Estimate the Loss matrix and EVPI at each WTP threshold
  for(l in 1:nrow(df_evpi)){
    
    # Compute NMB
    nmb <-  (df_evpi[l, "WTP"] * m_qalys) - m_costs
    
    # Identify the optimal strategy overall at this lambda
    meanChoice <- which.max(colMeans(nmb))
    
    # Calculate the difference in MB between:
    # 1. choosing one strategy based on the mean
    # 2. choosing the optimal strategy for every iteration
    mean_mb_loss <- mean( rowMaxs(as.matrix(nmb)) - nmb[, meanChoice] )
    
    df_evpi$EVPI[l] <- mean_mb_loss * population
    
  }

  
  # create a very simple plot
  plot(x = df_evpi$WTP,
       y = df_evpi$EVPI,
       type = "l",
       xlab = "Willingness to Pay per QALY (£)",
       ylab = "Population EVPI (£)")
  
  
  
  #=====================================================================================#
  
  # One way sensitivity analysis
  
  # since we have a PSA included in the function, we can vary one parameter at a time.
  
prices = 1:100 * 50
  
l_results <- lapply(X = as.list(prices),
         
                  FUN = function(x){
                    
                    # run the model with 1000 iterations
                    results <-  f_wrapper(c_Trt = x, n_sim = 1000)
                    
                    # calculate net benefit for each strategy
                    m_nb <- results[,c("QALY_NoTrt", "QALY_Trt")] * 25000 - results[,c("Cost_NoTrt", "Cost_Trt")]
                    
                    # what proportion of the 1000 iterations resulted in higher NB for treatment
                    p_CE <- sum( m_nb[2] > m_nb[1]) / 1000
                    
                    return(p_CE)
           
         })

# create a dataframe containing the results
df_results <- data.frame(p_CE = do.call(what = rbind, args = l_results),
                         price = prices)

# plot the results
plot(x = df_results$price, 
     y = df_results$p_CE*100,
     type = "b",
     xlab = "Price (£)",
     ylab = "% Probability Cost Effective at WTP = £25,000")
