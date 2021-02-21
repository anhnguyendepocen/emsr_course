rm(list = ls())

# CRAN DOWNLOADS
library(ggplot2)
library(data.table)
library(cranlogs)

# download and convert to data-table
dt <- as.data.table(cran_downloads(packages = c("heemod", "hesim", "BCEA", "ICEinfer", "survHE", "gems", "simmer","PROscorer","DALY"),
                     from = "2014-01-01"))

df <- dt[,.(cumulative = cumsum(count),
            date = date), 
         by = "package"] 
  


# create plot
p1 <- ggplot(data = as.data.frame(df), 
       aes(x = date, y = cumulative, col = package)) +
  theme_bw()+
  geom_line(size = 1.5)+
  ylab(label = "Cumulative downloads from CRAN")+
  xlab("")+
  scale_colour_discrete(name = "HE Package")+
  #scale_y_log10() +
  ggtitle(label = "Health Economics Package Downloads from CRAN (Cumulative)")

ggsave(filename = "HEdownloads.png", plot = p1, width = 10, height = 8)
