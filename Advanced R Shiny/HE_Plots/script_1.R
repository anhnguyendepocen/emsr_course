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

# first, run our markov model
results <- f_wrapper(n_age_init = 50,
                        n_age_max = 80,
                        d_r = 0.035,
                        n_sim = 10000,
                        c_Trt = 200)

# take what I need from the model output
TC = results[,c("Cost_NoTrt","Cost_Trt")]
TQ = results[,c("QALY_NoTrt","QALY_Trt")]

# define the values of lambda I want to calculate NB with
lambdas <- c(0:50) * 1000

# loop through the lambdas and calculate NB with each
INB = c()
for(l in lambdas){
  # loop through the lambdas and calculate NB given lambda for each iteration
  nb_temp = TQ * l - TC
  # calculate which is the maximum of the interventions
  inb_temp = apply(nb_temp,1,function(x) x == max(x))
  # store the number of times each were maximum
  inb_temp = apply(inb_temp,1,sum)
  # create a data-frame with the names of the interventions, lambda and the % CE
  inb_df_temp = data.frame(Intervention = c("No Treatment","Treatment"),
                           lambda = l,
                           value = inb_temp/sum(inb_temp))
  # add this as a new row to the data.frame
  INB = rbind(INB,inb_df_temp)
}

# take the INB data-frame and plot it
# plot
ggplot(data = INB, aes(x = lambda,
                       y= value,
                       col = Intervention) )+
  # add a line
  geom_line(size=1.5) + 
  
  # the y axis is continuous from 0-1 with breaks every 0.25
  scale_y_continuous(breaks=seq(0,1,0.25),
                     limits = c(0,1),
                     name = "Probability most cost-effective") +
  # add a label to the x axis
  xlab(label = "Willingness-to-pay (GBP)")+  
  # the theme is minimal
  theme_minimal() +
  # the legend is at the top
  theme(legend.position = "top") + 
  # labels are defined here
  labs(title = "Cost Effectiveness Acceptability Curve",
       subtitle = "The probability each preferred intervention is most cost effective against willingness to pay for each QALY threshold.") +
  # we end with a NULL for ease (that way we don't have to keep deleting the +)
  NULL





plotCEAC <- function(results = df_model_res){
  
  # take what I need from the model output
  TC = results[,c("Cost_NoTrt","Cost_Trt")]
  TQ = results[,c("QALY_NoTrt","QALY_Trt")]
  lambdas <- c(1:50) * 1000
  
  INB = c()
  for(l in lambdas){
    nb_temp = TQ * l - TC
    inb_temp = apply(nb_temp,1,function(x) x == max(x))
    inb_temp = apply(inb_temp,1,sum)
    inb_df_temp = data.frame(Intervention = c("No Treatment","Treatment"),
                             lambda = l,
                             value = inb_temp/sum(inb_temp))
    INB = rbind(INB,inb_df_temp)
  }
  
  
  # plot
  ggplot(data = INB, aes(x = lambda,
                         y= value,
                         col = Intervention) )+
    geom_line(size=1.5) + 
    
    scale_y_continuous(breaks=seq(0,1,0.25),
                       limits = c(0,1),
                       name = "Probability most cost-effective") +
    xlab(label = "Willingness-to-pay (GBP)")+  
    theme_minimal() +
    theme(legend.position = "top") + 
    labs(title = "Cost Effectiveness Acceptability Curve",
         subtitle = "The probability each preferred intervention is most cost effective against willingness to pay for each QALY threshold.") +
    NULL
  
}

# we can wrap plots into functions... it makes it much easier to 
# work with them
source("Advanced R Shiny/HE_Plots/plotCEAC.R")
source("Advanced R Shiny/HE_Plots/plotCEP.R")

# like this...
plotCEAC(results = f_wrapper(c_Trt = 100))
plotCEP(results = f_wrapper(c_Trt = 100))

# we can then change inputs & quickly reproduce plots
plotCEAC(results = f_wrapper(c_Trt = 700))
plotCEP(results = f_wrapper(c_Trt = 700))

# and quickly compare them ... we will do something more formal later
gridExtra::grid.arrange(
  plotCEAC(results = f_wrapper(c_Trt = 700))+
    ggtitle(label = "Cost = 700"),
  plotCEAC(results = f_wrapper(c_Trt = 100))+
    ggtitle(label = "Cost = 100"))



