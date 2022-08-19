library(ggplot2)
source('plot_survival_curve.R')
#### cph surgery####
data <- read.csv('data/recommendations/cph_surgery.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of surgery recommendations of cph.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### cph chemotherapy ####
data <- read.csv('data/recommendations/cph_chemotherapy.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of chemotherapy recommendations of cph.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### nmtlr surgery####
data <- read.csv('data/recommendations/nmtlr_surgery.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data) 
plot
tiff('Figures/survival curve of surgery recommendations of nmtlr.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### nmtlr chemotherapy ####
data <- read.csv('data/recommendations/nmtlr_chemotherapy.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of chemotherapy recommendations of nmtlr.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### deepsurv surgery####
data <- read.csv('data/recommendations/deepsurv_surgery.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of surgery recommendations of deepsurv.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### deepsurv chemotherapy ####
data <- read.csv('data/recommendations/deepsurv_chemotherapy.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of chemotherapy recommendations of deepsurv.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### rsf surgery####
data <- read.csv('data/recommendations/rsf_surgery.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of surgery recommendations of rsf.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
#### rsf chemotherapy ####
data <- read.csv('data/recommendations/rsf_chemotherapy.csv') %>% 
  subset(select=c(Survival.months,Status,Align)) %>%
  setNames(c('OS','status_os','Group')) %>%
  mutate(Group=Group%>%factor(labels=c('Anti-Recommendation','Recommendation')))
plot <- plot_surv(data)
tiff('Figures/survival curve of chemotherapy recommendations of rsf.tif',family = "Times New Roman",units = "cm",width = 20,height = 15,pointsize = 18,res = 300)
plot
dev.off()
