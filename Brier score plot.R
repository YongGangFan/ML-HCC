library(ggplot2)
#### Brier_score table ####
bs_data <- read.csv('data/recommendations/brier_score.csv')
bs_data %>% setNames(c('s','times','brier_scores','Models')) %>% ggplot(aes(x=times,y=brier_scores,color=Models)) +
  geom_line(size=1)+
  geom_line(aes(y=0.25),linetype=2,color=grey(0.5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 103),breaks = seq(0,100,10)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.26),breaks = seq(0.05,0.25,0.05))+
  annotate('text',x=10,y=0.24,label='0.25 limit')+
  ylab('Brier score')+
  xlab('Months')+
  theme(
    text=element_text(family='Times New Roman',size=15),
    axis.ticks = element_line(color = grey(0.5)),
    axis.line = element_line(color = grey(0.5)),
    axis.title = element_text(family='Times New Roman'),
    axis.text = element_text(family='Times New Roman',color='black'),
    panel.background = element_blank(),
    panel.grid.minor.y = element_line(),
    legend.position=c(0.7,0.2),
    legend.key = element_rect(colour = "transparent",fill = NA),
    legend.title = element_text(hjust = 0, family='Times New Roman'),
    legend.text = element_text(hjust = 0, family='Times New Roman')
    
  )
ggsave('./Figures/brier_scores.tiff',height = 5,width = 10,dpi = 300, device='tiff',  bg = "transparent")
