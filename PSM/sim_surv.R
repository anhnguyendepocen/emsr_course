
# simulate individual patient survival data
# install.packages("simsurv")
library(simsurv)


# ------- time horizon
# set horizon and time intervals
set.seed(123)
simdf <- data.frame(id= 1:200,trt = rep(c(1,0),each = 100))

os_wb_shape <- 1.424185
os_wb_scale <- 0.3074

pfs_wb_shape <- 1.55395
pfs_wb_scale <- 0.9290


####  OS
simdf_os <- simsurv(dist = "weibull",lambdas = os_wb_scale,gammas = os_wb_shape,x = simdf, betas = c(trt = 0.59), maxt = 10)
simdf_os <- merge(simdf,simdf_os)
simdf_os$trt <- ifelse(simdf_os$trt == 1, "SOC","Supimab")
write.csv(simdf_os, "./PSM/data/trial_os.csv",row.names = F)

####  PFS
simdf_pfs <- simsurv(dist = "weibull",lambdas = pfs_wb_scale,gammas = pfs_wb_shape,x = simdf, betas = c(trt = 0.9), maxt = 10)
simdf_pfs <- merge(simdf,simdf_pfs)
simdf_pfs$trt <- ifelse(simdf_pfs$trt == 1, "SOC","Supimab")
write.csv(simdf_pfs, "./PSM/data/trial_pfs.csv",row.names = F)