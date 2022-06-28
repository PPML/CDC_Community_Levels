#************************ MAKE ALTERNATIVE INDICATOR PLOTS ********************#
#*                                                                            *#
#*                                                                            *#
#*                                                                            *#
#******************************************************************************#
                                                                      
#### SETUP ####
library(here)
source(here("global_options.R"))

#### FUNCTIONS ####
format_data = function(source = "combined-county-2022-06-16-export.csv"){
  df2 = read.csv(source) %>% 
    gather(var, value, triggeron, triggeron2, triggeron3) %>%
    filter(value==1 & !is.na(value)) %>%
    mutate(ymd = as.Date(ymd, format = "%Y-%m-%d"),
           followtime = (max(ymd)-ymd)/7,
           lagcdc = ifelse(var=="triggeron2", lagcdc2, lagcdc3),
           event = lagcdc < followtime,
           event = ifelse(is.na(event), 0, event),
           stime = ifelse(lagcdc > followtime | is.na(lagcdc), followtime, lagcdc),
           lagzeke = ifelse(var=="triggeron2", zekelagalt1, zekelagalt2),
           lagzeke = ifelse(var=="triggeron", zekelagcdc, lagzeke),
           event_zeke = lagzeke < followtime,
           event_zeke = ifelse(is.na(event_zeke), 0, event_zeke),
           stime_zeke = ifelse(lagzeke > followtime | is.na(lagzeke), followtime, lagzeke))
  return(df2)
}


make_plots = function(df2, title1 = "", title2 = ""){

  # time to CDC red
  g = survfit(Surv(stime, event, type = "right")~var, type = "kaplan-meier", data = df2 %>% filter(var!="triggeron"))
  a = ggsurvplot(fit = g, xlab = "", ylab = "", title = title1,
             size = .3, censor.size = 0, xlim = c(.57, 12), legend = "none",
             palette = c("#56B4E9", "#009E73"), fun = "event", legend.title = "", break.x.by = 2,
             font.title = 12, 
             legend.labs = c("Alternative metric 1", "Alternative metric 2"))$plot + 
    #geom_vline(xintercept = 3, lty = 2, col = "darkgrey") + 
    theme(plot.title = element_text(hjust = 0.5))
  a
  
  # time to Zeke
  g2 = survfit(Surv(stime_zeke, event_zeke, type = "right")~var, type = "kaplan-meier", data = df2)
  b = ggsurvplot(fit = g2, xlab = "", ylab = "",  title = title2,
                 size = .3, censor.size = 0, xlim = c(.57, 12), legend = "bottom",
                 palette = c("#E59E00", "#56B4E9", "#009E73"), fun = "event", legend.title = "", break.x.by = 2,
                 font.title = 12, 
                 legend.labs = c("CDC High", "Alternative metric 1", "Alternative metric 2"))$plot + 
    geom_vline(xintercept = 3, lty = 2, col = "darkgrey") + 
    theme(plot.title = element_text(hjust = 0.5))
  b
  
  return(list(a,b))
}

# counties
df1 = format_data(source = here("0 - Data", "Archive-processed-2022-06-27", "combined-county-2022-06-27-export.csv"))
plots1 = make_plots(df1, title1 = "Time from new trigger to CDC High",
                    title2 = "Time from new trigger until deaths >0.9/100K/week")

# states
df2 = format_data(source =  here("0 - Data", "combined-state-2022-06-28-export.csv"))
plots2 = make_plots(df2)

# combine plots
e = ggarrange(plots1[[1]], plots1[[2]], legend = F) %>%
  annotate_figure(right = "Counties        ")
f = ggarrange(plots2[[1]], plots2[[2]], legend = F) %>% annotate_figure(right = "States             ")
g = ggarrange(e,f, ncol = 1) %>% annotate_figure(bottom = "Time (weeks)") %>% 
  ggarrange(legend_1 <- get_legend(plots2[[2]]), heights = c(2,.2),
         ncol = 1)
g

ggsave(here("2 - Figures", "survival_all.png"), width = 10, height = 8, units = "in")


# remake with titles for limited figure
plots1a = make_plots(df1, title1 = "",
                    title2 = "Counties")
plots2a = make_plots(df2, title1 = "",
                     title2 = "States")

h = ggarrange(plots1a[[2]], 
              plots2a[[2]], common.legend = T, legend = "bottom")
h

ggsave(here("2 - Figures", "survival_zeke.png"), width = 10, height = 4, units = "in")

