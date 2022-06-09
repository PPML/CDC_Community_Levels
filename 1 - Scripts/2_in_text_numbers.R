#### SETUP ####
library(here)
source(here("global_options.R"))
source(here("1 - Scripts", "1_make_plots.R"))

#### RESULTS SECTION NUMBERS ####

# number of new episodes
dim(d_out)

# mortality 21-days later
d_out %>% group_by(type) %>% 
  summarize(mean = mean(`Weekly deaths per 100K 21 days after start`, na.rm = T),
                    median = median(`Weekly deaths per 100K 21 days after start`, na.rm = T),
                    q25 = quantile(`Weekly deaths per 100K 21 days after start`, .25, na.rm = T),
                    q75 = quantile(`Weekly deaths per 100K 21 days after start`, .75, na.rm = T),
                    gt_0.9 = sum(`Weekly deaths per 100K 21 days after start`>.9, na.rm = T),
                    gt_2.1 = sum(`Weekly deaths per 100K 21 days after start`>2.1, na.rm = T),
                    gt_0.9_p = mean(`Weekly deaths per 100K 21 days after start`>.9, na.rm = T),
                    gt_2.1_p = mean(`Weekly deaths per 100K 21 days after start`>2.1, na.rm = T))


# mortality 21-days later by epoch
d_out %>% mutate(epoch = ifelse(`Start date`>="2021-11-01", "Omicron", "Delta")) %>%
  group_by(epoch, type) %>% summarize(n = n(),
                    mean = mean(`Weekly deaths per 100K 21 days after start`, na.rm = T),
                    median = median(`Weekly deaths per 100K 21 days after start`, na.rm = T),
                    q25 = quantile(`Weekly deaths per 100K 21 days after start`, .25, na.rm = T),
                    q75 = quantile(`Weekly deaths per 100K 21 days after start`, .75, na.rm = T),
                    quantile = quantile(max, .25, na.rm = T), max_peak = quantile(max, .75, na.rm = T))

# case fatality
# aggregate over US
us2 = us %>% group_by(ymd, dotw) %>% 
  filter(ymd>="2021-07-01") %>%
  summarize(
  num = sum(deaths_lag.21), denom = sum(cases_avg),
  admits = sum(admits_lag.21),
  hosps = sum(hosped_avg),
  cfr = num/denom*100,
  #cfr.dec1 = sum(deaths_deconvolved1)/denom*100,
  #cfr.dec2 = sum(deaths_deconvolved2)/denom*100,
  cfr.dec3 = sum(deaths_avg)/sum(cases_deconvolved1)*100,
  cfr.dec4 = sum(deaths_avg)/sum(cases_deconvolved2)*100,
  cfr.17 = sum(deaths_lag.17)/denom*100,
  hfr.14 = num/sum(admits_lag.14)*100,
  hfr.21 = num/sum(admits_lag.21)*100, 
  hosp_case = sum(admits_lag.21)/sum(cases_avg),
  hosp_case1 = sum(admits_lag.14)/sum(cases_avg)) %>% ungroup() %>%
  mutate(min_omi = cfr == min(cfr, na.rm = T),
         max_omi = cfr == max(cfr, na.rm = T)) %>%
  mutate(hfr.14 = lag(hfr.14, 7),
         cfr.lag = lag(cfr, 21),
         l_cases = log(denom),
         l_deaths_21 = log(num),
         l_admits = log(admits)) 

ggplot(us2, aes(x = hosp_case, y = cfr, col = ymd)) + geom_point() + geom_smooth() + 
  facet_grid()

ggplot(us2 %>% filter(dotw == "Wednesday"), aes(x = ymd, y = cfr-25*hosp_case)) + geom_point() + geom_smooth() + 
  facet_grid() + ylim(0, 30)

ggplot(us2 %>% #filter(dotw == "Wednesday") %>% 
         mutate(pred = 27*hosp_case-.4) %>%
         gather(var, value, cfr, pred), aes(x = ymd, y = value, group = var, col = var)) + 
  geom_point() +
  facet_grid() + ylim(0, 2.5)

summary(lm(cfr~cfr.lag + hosp_case, data = us2))
summary(lm(l_deaths_21~l_cases + l_admits + hosp_case, data = us2))

h = lm(l_deaths_21~l_cases + l_admits + hosp_case, data = us2 %>% filter(ymd >= "2022-04-01" & ymd <= "2022-05-01"))
us2$preds = exp(predict(h, newdata = us2))

g = lm(cfr~cfr.lag + hosp_case, data = us2  %>% filter(ymd >= "2022-04-01" & ymd <= "2022-05-01"))
us2$preds = predict(g, newdata = us2)

ggplot(us2 %>% #filter(dotw == "Wednesday") %>% 
         mutate(pred = 27*hosp_case-.4) %>%
         gather(var, value, cfr, pred, preds), aes(x = ymd, y = value, group = var, col = var)) + 
  geom_line() + 
  facet_grid() + ylim(0, 2.5)

ggplot(us2 %>% filter(dotw == "Wednesday" & ymd >= "2022-03-01") %>% 
         gather(var, value, num, preds), 
       aes(x = ymd, y = value, group = var, col = var)) + 
  geom_line() + 
  facet_grid()


# maximum
us2 %>% filter(max_omi) %>% mutate(ymd_shift = ymd + 21)

# minimum
us2 %>% filter(min_omi) %>% mutate(ymd_shift = ymd + 21)

# current
us2 %>% filter(ymd=="2022-03-15") %>% mutate(ymd_shift = ymd + 21)


#### MORE NATIONAL PLOTS ####
ggplot(us2, aes(x = ymd, y = cfr)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "21-day lagged COVID-19 case-fatality ratio (%)") + 
  geom_hline(yintercept = 1, lty = 3) + 
  geom_hline(yintercept = 2, lty = 3) +
  geom_hline(yintercept = .5, lty = 3) 
ggsave(here("2 - Figures", "cfr_national.png"), plot = last_plot(), width = 6, height = 4)

ggplot(us2 %>% gather(var, value, cfr.lag, cfr.dec3, cfr.dec4) %>% 
         mutate(lab = ifelse(var=="cfr.dec3", "Deconvolution w/17-day mean", "Deconvolution w/14-day mean"),
                lab = ifelse(var=="cfr.lag", "Unadjusted", lab)), 
       aes(x = ymd, y = value, group = lab, col = lab)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
 # scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  scale_color_brewer(name = "", palette = "Set1") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Lagged COVID-19 case-fatality ratio (%)")
ggsave(here("2 - Figures", "cfr_national_lags.png"), plot = last_plot(), width = 6, height = 4)


ggplot(us2 %>% gather(var, value, cfr, hosp_case) %>% group_by(var) %>%
         mutate(value2 = scale(value)), 
       aes(x = ymd, y = value2, group = var, col = var)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  # scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  scale_color_brewer(name = "", palette = "Set1") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Lagged COVID-19 case-fatality ratio (%)")


ggplot(us2 %>% gather(var, value, cfr, cfr.17) %>% 
         mutate(lab = ifelse(var=="cfr", "21-day lag", "17-day lag")), 
       aes(x = ymd, y = value, group = lab, col = lab)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  scale_color_brewer(name = "", palette = "Set1") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Lagged COVID-19 case-fatality ratio (%)") + 
  geom_hline(yintercept = 1, lty = 3) + 
  geom_hline(yintercept = 2, lty = 3) +
  geom_hline(yintercept = .5, lty = 3) 
ggsave(here("2 - Figures", "cfr_national_lags.png"), plot = last_plot(), width = 6, height = 4)


ggplot(us2 %>% gather(var, value, hfr.21, hfr.14) %>% 
         mutate(lab = ifelse(var=="hfr.21", "21-day lag", "14-day lag")),
       aes(x = ymd, y = value, group = lab, col = lab)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + ylim(0, 40) +
  #scale_y_continuous(breaks = c(0,.5, 1,2,3)) +
  scale_color_brewer(name = "", palette = "Set1") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Lagged COVID-19 hospital admission-fatality ratio (%)") +
  geom_hline(yintercept = 10, lty = 3) + 
  geom_hline(yintercept = 20, lty = 3) +
  geom_hline(yintercept = 30, lty = 3)
ggsave(here("2 - Figures", "hfr_national_lags.png"), plot = last_plot(), width = 6, height = 4)

ggplot(us2, aes(x = ymd, y = hosp_case)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + ylim(0, .2) +
  #scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Ratio of new hospital admissions to new cases") 
ggsave(here("2 - Figures", "hosp_case.png"), plot = last_plot(), width = 6, height = 4)

ggplot(us, aes(x = ymd, y = hosp_case_reg, group = state)) + geom_line(alpha = .5) +
  facet_wrap(.~region) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + ylim(0, .2) +
  #scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Ratio of new hospital admissions to new cases") 


us3 = us2 %>% filter(ymd>="2021-10-01") %>%
  gather(var, value, denom, admits, hosps) %>%
  mutate(lab = ifelse(var=="denom", "Cases", "New admissions"),
         lab = ifelse(var=="hosps", "Total hospitalized", lab)) %>%
  group_by(var) %>% mutate(value2 = scale(value)) 

ggplot(us3, 
       aes(x = ymd, y = value2, group = lab, col = lab)) + geom_line() + 
  #facet_wrap(.~lab, ncol = 1, scales = "free") + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) + 
  scale_color_discrete(name = "") +
  labs(x = "", y = "", title = "Standardized national COVID-19 cases, new admissions & total hospitalized")

ggplot(us3 %>% filter(ymd >= "2022-04-01"), 
       aes(x = ymd, y = value, group = lab, col = lab)) + geom_line() +
  facet_wrap(.~lab, ncol = 1, scales = "free") + ylim(0, NA)
  

## abstract plot

a = ggplot(us2 %>% filter(ymd>="2021-07-01" & ymd<="2022-04-07"), 
           aes(x = ymd, y = hosp_case)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + ylim(0, .2) +
  #scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Ratio of new hospital admissions to new cases") 


b = ggplot(us2 %>% filter(ymd>="2021-07-01" & ymd<="2022-04-07"),
       aes(x = ymd, y = hfr.21)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + ylim(0, 40) +
  #scale_y_continuous(breaks = c(0,.5, 1,2,3)) +
  scale_color_brewer(name = "", palette = "Set1") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "21-day lagged COVID-19 hospital admission-fatality ratio (%)") 

c = ggplot(us2 %>% filter(ymd>="2021-07-01" & ymd<="2022-04-07"), 
           aes(x = ymd, y = cfr)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  scale_y_continuous(breaks = c(0,.5, 1,2,3), limits = c(0, 3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "21-day lagged COVID-19 case-fatality ratio (%)") 

library(ggpubr)
ggarrange(a,b,c,ncol = 1)
ggsave("SMDM_abstract.png", width = 6, height = 9, unit = "in")
ggsave("SMDM_abstract.pdf", width = 6, height = 9, unit = "in")
