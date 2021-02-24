######## Tornado plot ###########

f_tornado <-function(results){
  
  require(ggplot2)
  require(reshape2)
  require(scales)
  
param_names <- c("Cost: Treatment", "Utility: S1", "Utility: Treatment S1")

m_determ<-matrix(results$ICER[-1], nrow = 2)

  # identify the mean (basecase ICER)
  ymean <- results$ICER[1]
  
  # Calculate the difference between the low and high values and the mean
  yMin <- m_determ[1, ] - ymean
  yMax <- m_determ[2, ] - ymean
  ySize <- abs(yMax - yMin)  # High value - Low value
  
  rankY<- order(ySize)
  n_Params <- length(param_names)
  
  df_determ_sa <- data.frame(
    Parameter = c(param_names[rankY], param_names[rankY]),  
    Level = c(rep("Low", n_Params), rep("High", n_Params)),
    value = ymean + c(yMin[rankY], yMax[rankY]),
    sort = seq(1, n_Params)
  )
  
  #Define offset as a new axis transformation. Source: http://blog.ggplot2.org/post/25938265813/defining-a-new-transformation-for-ggplot2-scales  
  offset_trans <- function(offset = 0) {
    trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x + offset)
  }
  
  #Plot the Tornado diagram
  g_tornado <- ggplot(df_determ_sa[df_determ_sa$Level == "Low", ], aes(x = Parameter, y = value, fill = level)) +
      geom_bar(stat = "identity", fill = "grey80") +
      ggtitle("Tornado Plot") +
      scale_y_continuous(name = "ICER (£)", trans = offset_trans(offset = ymean)) +
      scale_x_discrete(name = "Parameter") +
      geom_bar(data = df_determ_sa[df_determ_sa$Level == "High", ], aes(x = Parameter, y = value, fill = level), stat = "identity", fill = "coral", alpha = 0.5) +
      geom_hline(yintercept = ymean, linetype = "dotted", size = 0.5) +
      theme_bw(base_size = 14) +
      coord_flip() +
      theme(legend.position = "bottom")
   
  print(g_tornado)

}

source("Markov Model/src/f_gen_determ.R")
source("Markov Model/src/f_MM_sicksicker.R")
source("Markov Model/src/f_wrapper_determ.R")

results <- f_wrapper_determ()
f_tornado(results = results)
