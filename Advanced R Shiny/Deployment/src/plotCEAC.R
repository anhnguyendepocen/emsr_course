# --------------------------------------------------------------- #
#        COST EFFECTIVENESS ACCEPTABILITY CURVE PLOT              #
# --------------------------------------------------------------- #

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


