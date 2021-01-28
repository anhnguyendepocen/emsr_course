#########              Sick-Sicker Markov model                 #####################

#####################################################################################
## This code forms the basis for the cohort model of the article:                  ## 
## 'Microsimulation modeling for health decision sciences using R: a tutorial'     ##
## Authors: Eline Krijkamp, Fernando Alarid-Escudero,                              ##
##          Eva Enns, Hawre Jalal, Myriam Hunink and  Petros Pechlivanoglou        ##
## citation below                                                                  ##
#####################################################################################

# Developed by the Decision Analysis in R for Technologies in Health (DARTH) group
# Fernando Alarid-Escudero, PhD (1) 
# Eva A. Enns, MS, PhD (1)	
# M.G. Myriam Hunink, MD, PhD (2,3)
# Hawre J. Jalal, MD, PhD (4) 
# Eline M. Krijkamp, MSc (2)	
# Petros Pechlivanoglou, PhD (5) 

# In collaboration of: 		
# 1 University of Minnesota School of Public Health, Minneapolis, MN, USA
# 2 Erasmus MC, Rotterdam, The Netherlands
# 3 Harvard T.H. Chan School of Public Health, Boston, USA
# 4 University of Pittsburgh Graduate School of Public Health, Pittsburgh, PA, USA
# 5 The Hospital for Sick Children, Toronto and University of Toronto, Toronto ON, Canada

#####################################################################################
# Please cite our publications when using this code
# Jalal H, et al. An Overview of R in Health Decision Sciences. Med. Decis. Making. 2017; 37(3): 735-746. 
# Krijkamp EM, et al. Microsimulation modeling for health decision sciences using R: a tutorial. Med. Decis. Making. 2018;. 

#####################################################################################
# Copyright 2017, THE HOSPITAL FOR SICK CHILDREN AND THE COLLABORATING INSTITUTIONS. 
# All rights reserved in Canada, the United States and worldwide.  
# Copyright, trademarks, trade names and any and all associated intellectual property are exclusively owned by THE HOSPITAL FOR SICK CHILDREN and the 
# collaborating institutions and may not be used, reproduced, modified, distributed or adapted in any way without written permission.

#####################################################################################

rm(list = ls())  # delete everything that is in R's memory

#####################################################################################



Strategies <- c("No Treatment", "Treatment")  # strategy names 
age     <- 25                                 # age at baseline
max_age <- 55                                 # maximum age of follow up
n_t  <- max_age - age                         # time horizon, number of cycles
d_r <- 0.03                                   # equal discount of costs and QALYs by 3%

# Transition probabilities (per cycle)
p_HD    <- 0.005           # probability to die when healthy
p_HS1   <- 0.15          	 # probability to become sick when healthy
p_S1H   <- 0.5           	 # probability to become healthy when sick
p_S1S2  <- 0.105         	 # probability to become sicker when sick
hr_S1   <- 3             	 # hazard ratio of death in sick vs healthy
hr_S2   <- 10            	 # hazard ratio of death in sicker vs healthy 

# Cost and utility inputs 
c_H     <- 2000            # cost of remaining one cycle in the healthy state
c_S1    <- 4000            # cost of remaining one cycle in the sick state
c_S2    <- 15000           # cost of remaining one cycle in the sicker state
c_Trt   <- 12000           # cost of treatment(per cycle)
c_D     <- 0               # cost of being in the death state
u_H     <- 1               # utility when healthy
u_S1    <- 0.75            # utility when sick
u_S2    <- 0.5             # utility when sicker
u_D     <- 0               # utility when dead
u_Trt   <- 0.95            # utility when being treated

# rate of death in healthy
r_HD    <- - log(1 - p_HD) 

# rate of death in sick
r_S1D   <- hr_S1 * r_HD 	  
# rate of death in sicker
r_S2D   <- hr_S2 * r_HD  

# probability of death in sick
p_S1D   <- 1 - exp(-r_S1D) 
# probability of death in sicker
p_S2D   <- 1 - exp(-r_S2D) 

# calculate discount weight for each cycle
v_dwe <- v_dwc <- 1 / (1 + d_r) ^ (0:n_t)  # discount weight (equal discounting is assumed for costs and effects)

v_n  <- c("H", "S1", "S2", "D")               # the 4 states of the model: Healthy (H), Sick (S1), Sicker                                                  (S2), Dead (D)
n_states <- length(v_n)                            # number of health states 

############################### Markov Model  ###########################

#transition probability matrix for NO treatment
m_P <- matrix(data = 0,
              nrow = n_states, 
              ncol = n_states,
              dimnames = list(v_n, v_n))

m_P

### From Healthy
m_P["H", "H"]  <- 1 - (p_HS1 + p_HD)
m_P["H", "S1"] <- p_HS1
m_P["H", "D"]  <- p_HD
### From Sick
m_P["S1", "H"]  <- p_S1H
m_P["S1", "S1"] <- 1 - (p_S1H + p_S1S2 + p_S1D)
m_P["S1", "S2"] <- p_S1S2
m_P["S1", "D"]  <- p_S1D
### From Sicker
m_P["S2", "S2"] <- 1 - p_S2D
m_P["S2", "D"]  <- p_S2D
### From Dead
m_P["D", "D"] <- 1

# check rows add up to 1
rowSums(m_P)

m_P

# create empty Markov trace 
m_TR <- matrix(data = NA, 
               nrow = n_t + 1, 
               ncol = n_states, 
               dimnames = list(0:n_t, v_n)) 

head(m_TR)      # The head() function enables you to view the top of a table rather than the full matrix

# initialize Markov trace
m_TR[1, ] <- c(1, 0, 0, 0)    

head(m_TR)  # head shows us the first six rows by default. 

for (t in 1:n_t){ # throughout the number of cycles
  # estimate next cycle (t+1) of Markov trace
  m_TR[t + 1, ] <- m_TR[t, ] %*% m_P           
}

head(m_TR)  # head shows us the first six rows by default. 
