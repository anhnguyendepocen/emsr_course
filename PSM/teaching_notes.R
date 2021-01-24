

# PSM DEMO NOTES

# install.packages("flexsurv")
# install.packages("ggplot2")
library(flexsurv)
library(ggplot2)



# setup 
# utilities
u_pfs <- 0.90
u_pps <-  0.50

# costs soc
c_PFS_soc <- 22000

# costs supi
c_drug_supi <- 45000
c_admin_supi <- 2000
c_PFS_supi <- c_drug_supi + c_admin_supi

# costs supportive care for post-progression
c_support <- 5000


# ------- time horizon
# set horizon and time intervals
time_steps <- seq(from = 0, to = 10, by = 0.5)


# SUPIMAB survival curves 

# STEP 1 ------
    
    # OS
    os_supi_wb_const <- 0.3074
    os_supi_wb_scale <- 0.3536
    St_supi_os <- exp( -os_supi_wb_const * (time_steps^exp(os_supi_wb_scale)) )
    
    ggplot() +
      geom_line(aes(x= time_steps, y= St_supi_os))
    
    
    # S(t) Weibull function
    stWB <- function(const, scale, time){
      res <- exp( -const * (time^exp(scale)) )
      return(res)
    }
    
    St_supi_os <- stWB(const = os_supi_wb_const,scale = os_supi_wb_scale, time = time_steps)
    
  
# STEP 2  ------
    
    # PFS
    pfs_supi_wb_const <- 0.9290
    pfs_supi_wb_scale <- 0.4408
    St_supi_pfs <- stWB(pfs_supi_wb_const, pfs_supi_wb_scale, time_steps)
    
    # PPS = OS - PFS
    St_supi_pps <- St_supi_os - St_supi_pfs
    
    # dead = 1 - OS
    St_supi_dead <- 1 - St_supi_os
  

    
 # --------------------------------------------------- #    
###---------------###  EXERCISE 1 ###---------------### 
#--------------------------------------------------- #    
    
# SOC survival curves -------

    # OS
    os_soc_wb_const <- 0.4583
    os_soc_wb_scale <- 0.3249
    St_soc_os <-  stWB(os_soc_wb_const,  os_soc_wb_scale,  time_steps)
    
    # PFS
    pfs_soc_wb_const <- 1.8498
    pfs_soc_wb_scale <- 0.4337
    St_soc_pfs <- stWB(pfs_soc_wb_const, pfs_soc_wb_scale, time_steps)
    
    # PPS = OS - PFS
    St_soc_pps <- St_soc_os - St_soc_pfs
    
    # dead = 1 - OS
    St_soc_dead <- 1 - St_soc_os
    
### ------------------------------------------------------------------------------------ ###    
    
    # combine vectors of cumulative survival probabilities
    mat_surv <- cbind(
      St_supi_pfs,St_supi_pps,St_supi_dead,
      St_soc_pfs,St_soc_pps,St_soc_dead
    )
    
    mat_surv
    
    
    
# STEP 3 VIDUALISATION --------------
    
    # # proportion in state
    # ggplot() +
    #   geom_line(aes(x= time_steps, y = St_supi_os, col = "OS")) +
    #   geom_line(aes(x= time_steps, y = St_supi_pfs, col = "PFS")) +
    #   geom_line(aes(x= time_steps, y = St_supi_pps, col = "PPS")) +
    #   geom_line(aes(x= time_steps, y = St_supi_dead, col = "dead"))   +
    #   ggtitle("Supimab") +
    #   ylab("Proportion in state") +
    #   theme_minimal()
    
  # # visualise fitted survival curves
  # ggplot() +
  #   # SOC
  #   geom_point(aes(x = time_steps, y = St_soc_pfs, col = "SOC")) +
  #   geom_line(aes(x = time_steps, y = St_soc_pfs, linetype = "PFS", col = "SOC")) +
  #   geom_point(aes(x = time_steps, y = St_soc_os, col = "SOC")) +
  #   geom_line(aes(x = time_steps, y = St_soc_os, linetype = "OS",col = "SOC")) +
  #   # supimab
  #   geom_point(aes(x = time_steps, y = St_supi_pfs, col = "Supimab")) +
  #   geom_line(aes(x = time_steps, y = St_supi_pfs, linetype = "PFS",col = "Supimab")) +
  #   geom_point(aes(x = time_steps, y = St_supi_os, col = "Supimab")) +
  #   geom_line(aes(x = time_steps, y = St_supi_os, linetype = "OS",col = "Supimab")) +
  #   # labels, axes, and legend
  #   ggtitle("Supimab and SOC survival curves") +
  #   ylab("Cumulative survival") +
  #   scale_x_continuous(name = "Years", breaks = seq(0,10,2)) +
  #   scale_color_manual(name = "Treamtnet",
  #                      labels = c("SOC", "Supimab"),
  #                      values = c("#F8766D","#00BFC4")) +
  #   theme_minimal() +
  #   theme(legend.position = "bottom")
  # 

# STEP 4: AUC
    # visualise AUC PPS + PFS Supimab
    ggplot() +
      geom_ribbon(aes(x = time_steps, ymin = 0,ymax = St_supi_pfs, fill = "PFS")) +
      geom_ribbon(aes(x = time_steps, ymin = St_supi_pfs,ymax = St_supi_os, fill = "PPS"), alpha = 0.7) +
      geom_line(aes(x = time_steps, y = St_supi_pfs)) +
      geom_line(aes(x = time_steps, y = St_supi_os)) +
      # labels, axes, and legend
      ggtitle("PFS+PPS AUC - Supimab ") +
      ylab("Cumulative survival") +
      scale_x_continuous(name = "Years", breaks = seq(0,10,2)) +
      scale_fill_manual(name = "State", 
                        labels = c("PFS AUC","PPS AUC"),
                        values = c("#00BFC4","cadetblue")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # trapeziums illustration
    vline_df <- data.frame(cbind(
      x = rep(time_steps,2),
      y = c(cbind(0,St_supi_pfs)),
      id = as.factor(rep(time_steps,2))
    ))
    
    ggplot() +
      geom_ribbon(aes(x = time_steps[2:3], ymin = 0,ymax = St_supi_pfs[2:3], fill = "PFS")) +
      geom_line(aes(x = time_steps, y = St_supi_pfs)) +
      geom_line(aes(x = time_steps, y = St_supi_pfs)) +
      geom_line(data = vline_df, aes(x = x, y = y,group = id), linetype = "dashed") +
      geom_point(aes(
        x=c(time_steps[2:3],time_steps[2:3]),
        y=c(0,0,St_supi_pfs[2:3]))) +
      ggrepel::geom_label_repel(aes(
        x=c(time_steps[2:3],time_steps[2:3]),
        y=c(0,0,St_supi_pfs[2:3]),
        label = c("x1","x2","y1","y2")
      )) +
      geom_line(aes(
        x=c(0,time_steps[2]),
        y=c(St_supi_pfs[2],St_supi_pfs[2])
      ), linetype = "dotted") +
      geom_line(aes(
        x=c(0,time_steps[3]),
        y=c(St_supi_pfs[3],St_supi_pfs[3])
      ), linetype = "dotted") +
      theme_minimal() +
      xlim(c(0,4)) +
      geom_text(aes(x = 2.5,y = 0.65,label = "AUC = ( (y2+y1)/2)*(x2 - x1)"),size = 10) +
      theme(legend.position = "none")



# STEP 5 ---------------
    # # side step:
    # # we could have used a package instead
    # # install.packages("DescTools")
    # library(DescTools)
    # ?AUC
    # AUC # much shorter, but hard to read code
    # AUC(x=time_steps, y=St_supi_pps, method="trapezoid")
    
    
    # AUC = ((y2+y1)/2)*(x2 - x1)
    # example for one trapezium
    St_supi_pfs
    ((St_supi_pfs[3] + St_supi_pfs[2]) / 2 ) * (time_steps[3] - time_steps[2])


    # build your own AUC function
    myAUC <- function(x1,x2,y1,y2){
      res <- ((y2+y1)/2)*(x2 - x1)
      return(res)
    }
    
    
    # loop 0
    for(t in 1:length(time_steps)){ 
      cat("\n x1=",time_steps[t])
      cat("\n x2=",time_steps[t+1])
    }
    
    # loop 1 - through 1 St obj
    for(t in 1:(length(time_steps)-1)){ 
      cat("\n x1 =",time_steps[t])
      cat("; x2 =",time_steps[t+1])
    }
    
    
    # loop 2 - through 1 St obj
    mat_surv
    mat_surv[,"St_supi_pfs"]
    mat_surv[,1]
    mat_surv[1,1]
    
    for(t in 1:(length(time_steps)-1)){ 
        
        res <- myAUC(
          x1 = time_steps[t ],
          x2 = time_steps[t+1],
          y1 = mat_surv[t, 1],
          y2 = mat_surv[t+1,1]
          )
        
        print(res)
        
    }
    
    
    # loop 3 
    temp_auc <- c()
    for(t in 1:(length(time_steps) - 1 )){  
      #   t <- 1
      res <- myAUC(
        x1 = time_steps[t ],
        x2 = time_steps[t+1],
        y1 = mat_surv[t, 1],
        y2 = mat_surv[t+1,1]
      )
      
      temp_auc <- c(temp_auc, res)
      
    }
  
    temp_auc
    sum(temp_auc)
  
  
    # loop 4
    mat_auc <- c()
    for(i in 1:ncol(mat_surv)){
      
      temp_auc <- c()
      
      for(t in 1:(length(time_steps) - 1 )){  
        
        res <- myAUC(
          x1 = time_steps[t],
          x2 = time_steps[t+1],
          y1 = mat_surv[t,i],
          y2 = mat_surv[t+1, i]
        )
        
        temp_auc <- c(temp_auc, res)
      }
      
      mat_auc <- cbind(mat_auc,temp_auc)
      
    }
    
    mat_auc
    colSums(mat_auc)
    colnames(mat_auc) <- colnames(mat_surv)
    rownames(mat_auc) <- 1:nrow(mat_auc)
    rowSums(mat_auc) # time stpes check
    sum(mat_auc) # total auc check
    colSums(mat_auc) # time spend in state
    

    
    
#  NOT RUN -----------------  
  
    auc_df <- reshape2::melt(mat_auc)
    names(auc_df) <- c("time","state","auc")
    auc_df$trt <- grepl("supi", auc_df$state)
    auc_df$state <- gsub("soc_","",auc_df$state)
    auc_df$state <- gsub("supi_","",auc_df$state)
  
    ggplot(auc_df, aes(fill=state, y=auc, x=time)) +
      geom_bar(position="stack", stat="identity") +
      facet_wrap(~trt) +
      theme_minimal()

    
    


# STEP 6 -------------------------

  c_pfs_supi <- mat_auc[,"St_supi_pfs"] * (c_PFS_supi)
  c_pps_supi <- mat_auc[,"St_supi_pps"] * c_support
  c_pfs_soc <- mat_auc[,"St_soc_pfs"] * (c_PFS_soc)
  c_pps_soc <- mat_auc[,"St_soc_pps"] * c_support
  
  q_pfs_supi <- mat_auc[,"St_supi_pfs"] * u_pfs
  q_pps_supi <- mat_auc[,"St_supi_pps"] * u_pfs
  q_pfs_soc <- mat_auc[,"St_soc_pfs"] * u_pfs
  q_pps_soc <- mat_auc[,"St_soc_pps"] * u_pps
  
  # # shorter
  c_vector <- c(c_PFS_supi, c_support, 0, c_PFS_soc, c_support, 0)
  t(apply(mat_auc, 1, function(x){x* c_vector}))
  u_vector <- rep(c(u_pfs, u_pps, 0), 2)
  t(apply(mat_auc, 1, function(x){x * u_vector}))
  
  c_mat <- cbind(
    c_pfs_supi,c_pps_supi, 
    c_pfs_soc, c_pps_soc
  )
  
  q_mat <- cbind(
    q_pfs_supi,q_pps_supi,
    q_pfs_soc,q_pps_soc, 
  )
  
  
### STEP 7  ----------------------  
  # discounting
  # cost /(1 + disc_rate) ^ time
  # e.g. cost /(1.035) ^ 0.05
  
  myDiscounter <- function(x, time, disc_rate = 0.035){
    res <- x /(1 + disc_rate) ^ time 
    return(res)
  }
  
  myDiscounter(100,1)
  
  myDiscounter(c_pfs_supi,time = time_steps[-1])
  
  ##### EXERCISE #####
  # write a for loop to do this
  ##### EXERCISE #####
  
  c_mat_disc <- c()
  for(i in 1:ncol(c_mat)){
    temp <- myDiscounter(c_mat[,i],time_steps[-1])
    c_mat_disc <- cbind(c_mat,temp)
  }
  
  c_mat_disc <- apply(c_mat, 2, myDiscounter, time = time_steps[-1])
  
  q_mat_disc <- apply(q_mat,2,myDiscounter,time = time_steps[-1])
  q_mat_disc
  
  

# STEP 8 ---------------------------
    
    # costs supimab
    tc_supi <- sum(c_mat_disc[,c("c_pfs_supi","c_pps_supi")])
    tc_supi
    # qalys supimab
    tq_supi <-sum(q_mat_disc[,c("q_pfs_supi","q_pps_supi")])
    tq_supi
    # costs soc
    tc_soc <- sum(c_mat_disc[,c("c_pfs_soc","c_pps_soc")])
    tc_soc
    # qalys soc
    tq_soc <- sum(q_mat_disc[,c("q_pfs_soc","q_pps_soc")])
    tq_soc

    # ICER
    mean( (tc_supi - tc_soc) / (tq_supi - tq_soc) )
    
    # INB at 20k
    inb <-  (tq_supi - tq_soc) * 20000 - (tc_supi - tc_soc)
    inb
    
    # INB at 20k
    inb <-  (tq_supi - tq_soc) * 20000 - (tc_supi - tc_soc)
    inb


##### ADDITIONAL EXERCISE
    
calcInb <- function(c_1,c_2,q_1,q_2,wtp = 20000){
  res <-  (q_1 - q_2) * wtp - (c_1 - c_2)
  res
}

thres_vals <- seq(from = 0, to = 50000,by = 5000)

inb_vals <- calcInb(c_1 = 48243, q_1 = 1.8, c_2 = 17779,q_2 = 1,wtp = thres_vals)

ggplot() +
  geom_point(aes(x = thres_vals, y = inb_vals)) +
  geom_line(aes(x = thres_vals, y = inb_vals))  +
  geom_hline(yintercept = 0)
