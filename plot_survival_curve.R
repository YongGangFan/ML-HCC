library(magrittr)
library(dplyr)
library(ggplot2)
library(survminer)
library(survival)
library(egg)
library('ggpmisc')
library(tibble)
library(gtable)

plot_surv <- function(data,output_file){
  
  surv <- surv_fit(Surv(OS, status_os) ~ Group, data = data)
  surv_diff <- survdiff(Surv(OS, status_os) ~ Group, data = data)
  p.val = 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
  HR = (surv_diff$obs[2] / surv_diff$exp[2]) / (surv_diff$obs[1] / surv_diff$exp[1])
  up95 = exp(log(HR) + qnorm(0.975) * sqrt(1 / surv_diff$exp[2] + 1 / surv_diff$exp[1]))
  low95 = exp(log(HR) - qnorm(0.975) * sqrt(1 / surv_diff$exp[2] + 1 / surv_diff$exp[1]))
  OS_1 = data$OS[data$Group==data$Group%>%levels %>% .[1]]
  OS_2 = data$OS[data$Group==data$Group%>%levels %>% .[2]]
  sd_1 <- sd(OS_1, na.rm = FALSE) %>% round(2)
  sd_2 <- sd(OS_2, na.rm = FALSE) %>% round(2)
  mean_1 <- mean(OS_1) %>% round(2)
  mean_2 <- mean(OS_2) %>% round(2)
  surv_m <- surv %>% surv_median
  mOS_1 <- surv_m$median[2] %>% round(1) %>% format(nsmall = 1)
  mOS_2 <- surv_m$median[1] %>% round(1) %>% format(nsmall = 1)
  lower_1 <- surv_m$lower[2] %>% round(1) %>% format(nsmall = 1)
  lower_2 <- surv_m$lower[1] %>% round(1) %>% format(nsmall = 1)
  upper_1 <- surv_m$upper[2] %>% round(1) %>% format(nsmall = 1)
  upper_2 <- surv_m$upper[1] %>% round(1) %>% format(nsmall = 1)
  
  my_table <- data.frame(
    a = c(
      paste0(
        mOS_1, ' (', lower_1, '-', upper_1, ')'
      ),
      HR %>% round(2) %>% format(nsmall = 2) %>% paste0(' (',low95%>%round(2),'-',up95%>%round(2),')') ,
      ifelse(p.val > 0.001, p.val %>% round(3) %>% format(nsmall = 3), '<0.001')
    ),
    b = c(paste0(
      mOS_2, ' (', lower_2, '-', upper_2, ')'
    ),
    ' ',
    ' '),
    row.names = c(
      'mOS (95% CI), mo',
      'Hazard ratio (95% CI)',
      'P value (log-rank test)'
    ),
    check.names = F
  ) %>% setNames(c(data$Group%>%levels %>% .[2],data$Group%>%levels %>% .[1]))
  plot <- data %>%
    ggsurvplot(
      surv_fit(Surv(OS, status_os) ~ Group, data = .),
      data = . ,
      risk.table = T,
      palette = c('#385554', '#dc9644'),
      pval = F,
      risk.table.pos = 'out',
      # xlim = c(0, 24),
      break.time.by = 10,
      size = 1,
      tables.height = 0.25,
      xlab = "Months",
      tables.col = "strata",
      tables.y.text = F
    )
  # plot
  #61, 88, 86
  df <- tibble(x = 1,
               y = 0.5,
               tb = list(my_table %>% tibble()))
  plot$plot <- plot$plot +
    ylab('Overall survival')+
    theme(
      panel.grid.major.y = element_line(),
      axis.ticks = element_line(color = grey(0.5)),
      axis.line = element_line(color = grey(0.5)),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      text = element_text(size = 12,family='Times New Roman'),
      legend.position=c(0.85,1.1),
      legend.background = element_rect(colour = "transparent", fill = "white"),
      legend.title = element_blank()
      # axis.title.y  = element_text(margin = margin(t = 0, r = -100, b = 0, l = 0)),
      # plot.margin = margin(t = 0, r = 0, b = 0, l = 50)
    ) 
  
  # plot
  plot$table <- plot$table +
    theme(
      plot.title = element_text(hjust = 0, size = 12,family='Times New Roman'),
      axis.ticks = element_line(color = grey(0.5)),
      axis.line = element_line(color = grey(0.5)),
      axis.title = element_text(size = 12,family='Times New Roman'),
      axis.title.y = element_text(hjust = -100),
      axis.text = element_text(size = 12,family='Times New Roman'),
      text = element_text(size = 12,family='Times New Roman'),
      legend.position="none",
      legend.title = element_text(hjust = 0, size = 12,family='Times New Roman'),
      legend.text = element_text(hjust = 0, size = 12,family='Times New Roman')
      
    )
  # plot
  rect_grob <- function (fill = "white", col = "black", lty = "solid", lwd = 1, 
                         cex = 1, alpha = 1, lineend = "round", linejoin = "round", 
                         linemitre = 10, lex = 1, name = NULL, vp = NULL, just = "centre", 
                         hjust = 0.5, vjust = 0.5, width = unit(1, "npc") - unit(2, 
                                                                                 "scaledpts"), height = unit(1, "npc") - unit(2, "scaledpts"), 
                         x = 0.5, y = 0.5, default.units = "npc") 
  {
    rectGrob(x = x, y = y, just = just, hjust = hjust, vjust = vjust, 
             width = width, height = height, default.units = default.units, 
             name = name, vp = vp, gp = gpar(col = col, fill = fill, 
                                             alpha = alpha, lty = lty, lwd = lwd, lex = lex, 
                                             lineend = lineend, linejoin = linejoin, linemitre = linemitre, 
                                             cex = cex))
  }
  
  # c = tableGrob(my_table,theme = ttheme(rownames.style = rownames_style(face = 'plain',size = 12,hjust=0,x=0.1,fill = 'white'),
  #                                       base_style = 'light',
  #                                       padding = unit(c(5, 4), "mm",),
  #                                       base_colour='black',
  #                                       base_size = 12,
  #                                       tbody.style = tbody_style(size = 12,fill='white'),
  #                                       colnames.style = colnames_style(size = 12,fill = 'white')
  # ))%>%
  #   tab_add_hline(at.row = 1, row.side = "bottom", linewidth = 2) %>%
  #   tab_add_hline(at.row = 2:4, row.side = "bottom", linewidth = 1,linecolor = gray(0.8))
  my_theme <- ttheme_gtminimal(
    base_family = "Times New Roman",
    base_size = 12,
    padding = unit(c(5, 4), "mm")
  )
  my_theme$rowhead$fg_params$fontface <- 1
  my_theme$rowhead$fg_params$hjust <- 0
  my_theme$rowhead$fg_params$x <- 0.1
  c = tableGrob(my_table,theme = my_theme)%>%
    tab_add_hline(at.row = 1, row.side = "bottom", linewidth = 2) %>%
    tab_add_hline(at.row = 2:4, row.side = "bottom", linewidth = 1,linecolor = gray(0.8))
  gt <- gtable("demo",respect = F, width=unit(c(10,10), c('cm',"cm")),heights = unit(c(3,10,4), "null"))
  c <- gtable_add_padding(c,unit(c(0,0,0,5), "cm"))
  gt <- gtable_add_grob(gt,
                        list(c,ggplotGrob(plot$plot), 
                             gtable_add_padding(ggplotGrob(plot$table),unit(c(0,0,0,0.15), "cm"))), 
                        t = c(1, 2, 3),
                        l = c(1, 1, 1),
                        r = c(1,2,2),
                        clip = 'off')
  return(as_ggplot(gt))
  # # return{as_ggplot(gt)}

}

