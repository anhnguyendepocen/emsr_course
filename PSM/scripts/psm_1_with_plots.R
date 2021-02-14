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
times <- seq(from = 0, to = 10, by = 1/52) # NOTE: 1 interval = 1 week

# utilities
u_pfs <- 0.90
u_pps <-  0.65

# costs soc
c_soc <- 22000

# costs supimab
c_drug_supi <- 30000
c_admin_supi <- 2000
c_supi <- c_drug_supi + c_admin_supi

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



#----------------------------------------------------#
### visual inpsection of curves ------
kmPlotter <- function(event_times, group){
  # simplified KM plot df function
  # can only be used when there is NO CENSORING!
  uniq_g <- unique(group)
  res<- c()
  for(g in uniq_g){
    times <- event_times[group ==g]
    min_t <- min(times)
    max_t <- max(times)
    uniq_t <- unique(times)
    uniq_t <- uniq_t[order(uniq_t)]
    n <- length(times)
    S <- c(1)
    for(t in 2:length(uniq_t)){
      post_n <- sum(times > uniq_t[t])
      S <- c(S,post_n/ n  )
    }
    df <- data.frame(times = uniq_t, S, trt = g) 
    res <- rbind(res,df)
  }
  
  return(res)
}

km_plot_os <- kmPlotter(df_os$eventtime,df_os$trt)
km_plot_pfs <- kmPlotter(df_pfs$eventtime,df_pfs$trt)


# OS ONLY
ggplot() +
  geom_step(data = km_plot_os,aes(x=times, y= S, col = trt, linetype="OS"), alpha = 0.5) +
  geom_line(aes(x=times,y=pred_os_soc,col = "SOC", linetype="OS" )) +
  geom_line(aes(x=times,y=pred_os_supi, col="Supimab",linetype="OS" )) +
  coord_cartesian(xlim=c(0,7)) +
  theme_minimal()


# PFS ONLY
ggplot() +
  geom_step(data = km_plot_pfs,aes(x=times, y= S, col = trt, linetype="PFS"), alpha = 0.5) +
  geom_line(aes(x=times,y=pred_pfs_soc,col = "SOC", linetype="PFS" )) +
  geom_line(aes(x=times,y=pred_pfs_supi, col="Supimab",linetype="PFS" )) +
  coord_cartesian(xlim=c(0,7)) +
  theme_minimal()

# TOGETHER
ggplot() +
  # OS KM observed
  geom_step(data = km_plot_os,aes(x=times, y= S, col = trt, linetype="OS"), alpha = 0.5) +
  # PFS KM observed
  geom_step(data = km_plot_pfs,aes(x=times, y= S, col = trt, linetype="PFS"), alpha = 0.5) +
  
  # Weibull est OS
  geom_line(aes(x=times,y=pred_os_soc,col = "SOC", linetype="OS" )) +
  geom_line(aes(x=times,y=pred_os_supi, col="Supimab",linetype="OS" )) +
  # Weibull est PFS
  geom_line(aes(x=times,y=pred_pfs_soc,col = "SOC", linetype="PFS" )) +
  geom_line(aes(x=times,y=pred_pfs_supi, col="Supimab",linetype="PFS" )) +
  
  coord_cartesian(xlim=c(0,7)) +
  theme_minimal()

# AUC
## visualise AUC PPS + PFS Supimab
ggplot() +
  geom_ribbon(aes(x = times, ymin = 0,ymax = pred_pfs_supi, fill = "PFS")) +
  geom_ribbon(aes(x = times, ymin = pred_pfs_supi,ymax = pred_os_supi, fill = "PPS"), alpha = 0.7) +
  geom_line(aes(x = times, y = pred_pfs_supi)) +
  geom_line(aes(x = times, y = pred_os_supi)) +
  ## labels, axes, and legend
  ggtitle("PFS+PPS AUC - Supimab ") +
  ylab("Cumulative survival") +
  scale_x_continuous(name = "Years", breaks = seq(0,10,2)) +
  scale_fill_manual(name = "State", 
                    labels = c("PFS AUC","PPS AUC"),
                    values = c("#00BFC4","cadetblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")


# AUC
## visualise AUC PPS + PFS SOC
ggplot() +
  geom_ribbon(aes(x = times, ymin = 0,ymax = pred_pfs_soc, fill = "PFS")) +
  geom_ribbon(aes(x = times, ymin = pred_pfs_soc,ymax = pred_os_soc, fill = "PPS"), alpha = 0.7) +
  geom_line(aes(x = times, y = pred_pfs_soc)) +
  geom_line(aes(x = times, y = pred_os_soc)) +
  ## labels, axes, and legend
  ggtitle("PFS+PPS AUC - SOC ") +
  ylab("Cumulative survival") +
  scale_x_continuous(name = "Years", breaks = seq(0,10,2)) +
  scale_fill_manual(name = "State", 
                    labels = c("PFS AUC","PPS AUC"),
                    values = c("#00BFC4","cadetblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

#----------------------------------------------------#








# 6 Define and apply AUC function
myAUC <- function(x, y){
  auc <- rep(NA, times = length(y)-1)
  for (i in 2:(length(x))) {
    auc_i <- ( (y[i] + y[i - 1])/2 ) * (x[i] - x[i - 1])
    auc[i-1] <- auc_i
  }
  return(auc)
}

mat_auc <- apply(mat_surv,2,myAUC,x = times)
# mat_auc

## Run some basic checks  
# rowSums(mat_auc) # time stpes check
# sum(mat_auc) # total auc check
# colSums(mat_auc) # time spend in state


#----------------------------------------------------#
### visual inpsection of AUCs ------
auc_df <- reshape2::melt(mat_auc)
names(auc_df) <- c("time","state","auc")
auc_df$trt <- ifelse(grepl("supi", auc_df$state),"supimab","soc")
auc_df$state <- gsub("_soc","",auc_df$state)
auc_df$state <- gsub("_supi","",auc_df$state)

ggplot(auc_df, aes(fill=state, y=auc, x=time)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~trt) +
  theme_minimal()
#----------------------------------------------------#



# 7 multiply costs and utilities with computed AUCs
c_pfs_supi <- mat_auc[,"pred_pfs_supi"] * c_supi
c_pps_supi <- mat_auc[,"pred_pps_supi"] * c_support
c_pfs_soc <- mat_auc[,"pred_pfs_soc"] * c_soc
c_pps_soc <- mat_auc[,"pred_pps_soc"] * c_support

q_pfs_supi <- mat_auc[,"pred_pfs_supi"] * u_pfs
q_pps_supi <- mat_auc[,"pred_pps_supi"] * u_pps
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

#----------------------------------------------------#
### visual inpsection of icer with 20k threshold ------

ggplot() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_abline(slope = 20000, linetype = "dashed") +
  geom_point(aes(x = total_qalys_supi - total_qalys_soc,y = total_costs_supi - total_costs_soc), col ="magenta", size = 3) +
  theme_minimal() +
  labs(title = "Cost-effectiveness Plane") +
  xlab("Incremental QALYs") + 
  ylab("Incremental Costs") 
  
#----------------------------------------------------#
  
## INB at 20k
inb_20 <-  (total_qalys_supi - total_qalys_soc) * 20000 - (total_costs_supi - total_costs_soc)
inb_20

## INB at x
wtp_steps <- seq(0,50000,1000)
inb_x <- (total_qalys_supi - total_qalys_soc) * wtp_steps - (total_costs_supi - total_costs_soc)
cbind(wtp_steps,inb_x)

