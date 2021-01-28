ce_plot <- function(results = df_model_res,
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
               fill="red",size=5,pch=21) +
    geom_abline(intercept = 0,linetype="dashed", slope = wtp)+ # add abline based on wtp
    
    
    
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




# --------------------------------------------------------------- #
#        COST EFFECTIVENESS ACCEPTABILITY CURVE PLOT              #
# --------------------------------------------------------------- #

ceac_plot <- function(results = df_model_res){
  
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


