# FUNCTION TO GENERATE ONE WAY SENSITIVITY ANALYSIS INPUTS



f_gen_determ <- function(c_Trt = 50, l_varSA = c("c_Trt", "u_S1", "u_Trt")){

  n_var <- length(l_varSA)
  l_sa <- paste0(l_varSA, c(rep("_low", n_var), rep("_high", n_var)))
  row_names <- c("basecase", l_sa)
  
  df_determ <- data.frame(
  
  p_HD  = c(rep(x = 0.005,times = length(row_names))),          # probability to die when healthy
  p_HS1 = 0.15,          	 # probability to become sick when healthy
  p_S1H = 0.5,           	 # probability to become healthy when sick
  p_S1S2 = 0.105,         	 # probability to become sicker when sick
  hr_S1  = 3,             	 # hazard ratio of death in sick vs healthy
  hr_S2  = 10,            	 # hazard ratio of death in sicker vs healthy 
  
  # Cost and utility inputs 
  c_H     = 2000,            # cost of remaining one cycle in the healthy state
  c_S1    = 4000,            # cost of remaining one cycle in the sick state
  c_S2    = 15000,           # cost of remaining one cycle in the sicker state
  c_Trt   = c_Trt,           # cost of treatment(per cycle; S1,S2)
  c_D     = 0,               # cost of being in the death state
  u_H     = 1,               # utility when healthy
  u_S1    = 0.75,            # utility when sick
  u_S2    = 0.5,             # utility when sicker
  u_D     = 0,               # utility when dead
  u_Trt   = 0.95            # utility when being treated (S1)
  
  )
  
  row.names(df_determ)<-row_names
  
  for(i in 1:length(l_varSA)){
    
  df_determ[i*2,l_varSA[i]]<-df_determ[i*2,l_varSA[i]]*0.9
  df_determ[1+(i*2),l_varSA[i]]<-df_determ[1+(i*2),l_varSA[i]]*1.1
  
  
  df_determ
  }
 
  return(df_determ)
}

