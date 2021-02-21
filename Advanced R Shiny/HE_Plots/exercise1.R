# ==============
# Making Health Economic Models Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# University of Sheffield & Dark Peak Analytics
# contact: info@darkpeakanalytics.com
# ==============
rm(list = ls())

library(ggplot2)

# source the scripts that contain the model functions from the Markov Model/src folder.
source("Markov Model/src/f_gen_psa.R")
source("Markov Model/src/f_MM_sicksicker.R")
source("Markov Model/src/f_wrapper.R")

# 1. run the wrapper function to create a set of results.
# store these results in a data.frame called df_results

df_results <- f_wrapper() # < insert code here > #


# calculate incremental costs and qalys for each PSA iteration and store in a data-frame
df_plot <- data.frame(inc_C = df_results$Cost_Trt - df_results$Cost_NoTrt,
                      inc_Q = df_results$QALY_Trt - df_results$QALY_NoTrt)

# crate a mean of the incremental costs and qalys.
means <- data.frame(mean_inc_C = mean(df_plot$inc_C),
                    mean_inc_Q = mean(df_plot$inc_Q))

# create an empty plot but which is going to refer to the data.frame df_plot we just created.
cep_plot <- ggplot(data = df_plot,
                   aes(x = inc_Q,     # x axis incremental QALYS
                       y = inc_C))    # y axis incremental Costs +
  
# view the plot
cep_plot

# 2. Create a simple scatter plot using ggplot2 that has inc_Q on the x axis 
#    and inc_C on the y axis and each point representing an iteration.

cep_plot <- cep_plot + 
  
  # < insert code here > # +
  NULL

# view the plot
cep_plot

#====================================================#
# 3. Insert a large red point to represent the mean Costs & QALYs
cep_plot <- cep_plot + 
  
  # < insert code here > # +
  
  NULL
  
# view the plot
cep_plot 


# 4. Add a Title, Subtitle, x axis title and y axis title
cep_plot = cep_plot +
  
  # < insert code here > # +
  
  NULL

# view the plot
  cep_plot  
  
  
  
# 5. Review the following function, can you quickly change the theme to be theme_dark?
  plotCEP <- function(results = df_model_res,
                      wtp = 20000){
    
    # calculate incremental costs and qalys from results data-frame in function input.
    df_plot <- data.frame(inc_C = results$Cost_Trt - results$Cost_NoTrt,
                          inc_Q = results$QALY_Trt - results$QALY_NoTrt)
    
    means <- data.frame(mean_inc_C = mean(df_plot$inc_C),
                        mean_inc_Q = mean(df_plot$inc_Q))
    
    # now use this plotting data-frame to create a very simple ggplot.
    plot <- ggplot(data = df_plot,
                   aes(x = inc_Q,  # x axis incremental QALYS
                       y = inc_C)  # y axis incremental Costs
    ) +
      theme_minimal()  +
      
      # titles
      labs(title = "Cost-effectiveness Plane",
           subtitle = paste0("Results of Probabilistic Sensitivity Analysis")) +
      xlab("Incremental QALYs") +
      ylab("Incremental Costs (GBP)") +
      
      
      # add points and ellipse and horizontal and vertical lines
      geom_point(alpha = 0.5,size = 0.7) +
      stat_ellipse(type="norm", level=0.9,
                   segments =50,col= "red") +
      geom_vline(aes(xintercept = 0), colour = "grey",)+
      geom_hline(aes(yintercept = 0),colour="grey") +
      geom_point(data = means, 
                 aes(x=mean_inc_Q,
                     y=mean_inc_C),
                 fill="red", size=5, pch=21) +
      geom_abline(intercept = 0, linetype="dashed", slope = wtp)+ # add abline based on wtp
      
      # set x-limits and y-limits for plot.
      xlim(c(
        min(df_plot$inc_Q, df_plot$inc_Q * -1),
        max(df_plot$inc_Q, df_plot$inc_Q * -1)
      )) +
      
      ylim(c(
        min(df_plot$inc_C, df_plot$inc_C * -1),
        max(df_plot$inc_C, df_plot$inc_C * -1)
      ))
    
    plot # output the plot from the function.
    
  }
 

# 6. A last minute change to the price of the treatment has been announced.
  # Run the model with c_Trt = 100 and plot the results
  
  # HINT: NESTED FUNCTIONS

  

  


