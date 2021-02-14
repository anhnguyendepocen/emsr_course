#==========================#
# Making Health Economic Evaluation Shiny: A tutorial
# Robert Smith, Paul Schneider & Sarah Bates
# Dark Peak Analytics
# Feb 2021
#==========================#

# PSM: partition survival modelling
  #
  # New drug 'supimab' vs standard of care ('soc')
  # A 3-state model (PFS, PPS, DEAD)
  # Curves based on parametric survival models and IPD


# 1 load packages
  library(survival)
  library(flexsurv)
  library(ggplot2)


# 2 Define key model parameters  
  
  # Define horizon and time intervals 
  times <- seq(from = 0, to = 10, by = 1/365) # NOTE: 1 interval = 1 day 
  
  # utilities
  u_pfs <- 0.90
  u_pps <-  0.50
  
  # costs soc
  c_pfs_soc <- 22000
  
  # costs supimab
  c_drug_supi <- 45000
  c_admin_supi <- 2000
  c_pfs_supi <- c_drug_supi + c_admin_supi
  
  # costs supportive care for post-progression
  c_support <- 5000
  
  # discount rate
  disc_rate = 0.035

  
# 3 load data and fit parametric models --------
  ## OS
  df_os <- read.csv("./PSM/data/trial_os.csv")
  # head(df_os)
  ## create OS survical obj 
  surv_os <- Surv(time = df_os$eventtime, event = df_os$status) 
  ## fit parametric survival model (weibull proportionate hazards Model)
  os_reg <- flexsurvreg(surv_os ~ trt, data = df_os,dist = "weibullph")
  # os_reg

  ## PFS
  df_pfs <- read.csv("./PSM/data/trial_pfs.csv")
  # head(df_pfs)
  ## create PFS survical obj 
  surv_pfs <- Surv(time = df_pfs$eventtime, event = df_pfs$status)
  ## fit parametric PFS survival model (weibull proportionate hazards Model)
  pfs_reg <- flexsurvreg(surv_pfs ~ trt, data = df_pfs,dist = "weibullph")
  # pfs_reg

  
# 4 retrieve parameters from models 
  os_shape <- os_reg$res[1,1]
  os_scale_soc <- os_reg$res[2,1]
  os_scale_supi <- os_reg$res[2,1] * exp(os_reg$res[3,1])
  
  pfs_shape <- pfs_reg$res[1,1]
  pfs_scale_soc <- pfs_reg$res[2,1]
  pfs_scale_supi <- pfs_reg$res[2,1] * exp(pfs_reg$res[3,1])
  
  
# 5 Predict cumulative survival at given time points
  ## OS curves
  pred_os_soc <- 1 - os_reg$dfns$p(times,shape=os_shape,scale = os_scale_soc)
  pred_os_supi <- 1 - os_reg$dfns$p(times,shape=os_shape, scale = os_scale_supi)
  
  ## PFS curves
  pred_pfs_soc <- 1 - pfs_reg$dfns$p(times,shape=pfs_shape,scale = pfs_scale_soc)
  pred_pfs_supi <- 1 - pfs_reg$dfns$p(times,shape=pfs_shape, scale = pfs_scale_supi)
  
  ## PPS curves
  pred_pps_soc <- pred_os_soc - pred_pfs_soc
  pred_pps_supi <- pred_os_supi - pred_pfs_supi
  
  ## dead curves
  pred_dead_soc <- 1 - pred_os_soc
  pred_dead_supi <- 1 - pred_os_supi

  # Combine estimated survival curves into a matrix
  mat_surv <- cbind(
    pred_pfs_soc, pred_pps_soc, pred_dead_soc,
    pred_pfs_supi, pred_pps_supi, pred_dead_supi
  )
  # mat_surv

  
# 6 Define and apply AUC function
  myAUC <- function(y,time){
    auc <- rep(NA, times = length(y)-1)
    for (i in 2:(length(time))) {
      auc_i <- ( (y[i] + y[i - 1])/2 ) * (time[i] - time[i - 1])
      auc[i-1] <- auc_i
    }
    return(auc)
  }
  
  mat_auc <- apply(mat_surv,2,myAUC,time = times)
  # mat_auc

  ## Run some basic checks  
  # rowSums(mat_auc) # time stpes check
  # sum(mat_auc) # total auc check
  # colSums(mat_auc) # time spend in state



# 7 multiply costs and utilities with computed AUCs
  c_pfs_supi <- mat_auc[,"pred_pfs_supi"] * (c_pfs_supi)
  c_pps_supi <- mat_auc[,"pred_pps_supi"] * c_support
  c_pfs_soc <- mat_auc[,"pred_pfs_soc"] * (c_pfs_soc)
  c_pps_soc <- mat_auc[,"pred_pps_soc"] * c_support
  
  q_pfs_supi <- mat_auc[,"pred_pfs_supi"] * u_pfs
  q_pps_supi <- mat_auc[,"pred_pps_supi"] * u_pfs
  q_pfs_soc <- mat_auc[,"pred_pfs_soc"] * u_pfs
  q_pps_soc <- mat_auc[,"pred_pps_soc"] * u_pps

  # combines costs and qalys into matrices
  c_mat <- cbind(c_pfs_supi, c_pps_supi, c_pfs_soc, c_pps_soc)
  q_mat <- cbind(q_pfs_supi, q_pps_supi, q_pfs_soc, q_pps_soc)


# 8 Define discount function and apply it to costs and qalys
  myDiscounter <- function(x, time, rate = 0.035){
    res <- x /(1 + rate) ^ time 
    return(res)
  }
  
  c_mat_disc <- apply(c_mat, 2, myDiscounter, time = times[-1])
  # c_mat_disc
  q_mat_disc <- apply(q_mat,2,myDiscounter,time = times[-1])
  # q_mat_disc


# 9 Sum total (discounted) costs and qalys and compute ICER + INB
  
  ## costs supimab
  total_costs_supi <- sum(c_mat_disc[,c("c_pfs_supi","c_pps_supi")])
  # total_costs_supi
  
  ## costs soc
  total_costs_soc <- sum(c_mat_disc[,c("c_pfs_soc","c_pps_soc")])
  # total_costs_soc
  
  ## qalys supimab
  total_qalys_supi <-sum(q_mat_disc[,c("q_pfs_supi","q_pps_supi")])
  # total_qalys_supi
  
  ## qalys soc
  total_qalys_soc <- sum(q_mat_disc[,c("q_pfs_soc","q_pps_soc")])
  # total_qalys_soc
  
  ## ICER
  mean( (total_costs_supi - total_costs_soc) / (total_qalys_supi - total_qalys_soc) )
  
  ## INB at 20k
  inb_20 <-  (total_qalys_supi - total_qalys_soc) * 20000 - (total_costs_supi - total_costs_soc)
  # inb_20
  
  ## INB at x
  wtp_steps <- seq(0,50000,1000)
  inb_x <- (total_qalys_supi - total_qalys_soc) * wtp_steps - (total_costs_supi - total_costs_soc)
  # inb_x