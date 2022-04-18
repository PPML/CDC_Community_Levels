#### SETUP ####
library(here)
source(here("global_options.R"))
source(here("1 - Scripts", "1_make_plots.R"))

#### RESULTS SECTION NUMBERS ####

# number of new episodes
dim(d_out)

# mortality 21-days later
d_out %>% summarize(mean = mean(`Weekly deaths per 100K 21 days after start`),
                    median = median(`Weekly deaths per 100K 21 days after start`),
                    q25 = quantile(`Weekly deaths per 100K 21 days after start`, .25),
                    q75 = quantile(`Weekly deaths per 100K 21 days after start`, .75),
                    gt_0.9 = sum(`Weekly deaths per 100K 21 days after start`>.9),
                    gt_2.1 = sum(`Weekly deaths per 100K 21 days after start`>2.1),
                    gt_0.9_p = mean(`Weekly deaths per 100K 21 days after start`>.9),
                    gt_2.1_p = mean(`Weekly deaths per 100K 21 days after start`>2.1))


# mortality 21-days later by epoch
d_out %>% mutate(epoch = ifelse(`Start date`>="2021-11-01", "Omicron", "Delta")) %>%
  group_by(epoch) %>% summarize(n = n(),
                    mean = mean(`Weekly deaths per 100K 21 days after start`),
                    median = median(`Weekly deaths per 100K 21 days after start`),
                    q25 = quantile(`Weekly deaths per 100K 21 days after start`, .25),
                    q75 = quantile(`Weekly deaths per 100K 21 days after start`, .75),
                    quantile = quantile(max, .25, na.rm = T), max_peak = quantile(max, .75, na.rm = T))

# case fatality
# aggregate over US
us2 = us %>% group_by(ymd) %>% 
  filter(ymd>="2021-06-01") %>%
  summarize(
  num = sum(deaths_lag.21), denom = sum(cases_avg),
  admits = sum(admits_lag.21),
  hosps = sum(hosped_avg),
  cfr = num/denom*100,
  cfr.17 = sum(deaths_lag.17)/denom*100,
  hfr.14 = num/sum(admits_lag.14)*100,
  hfr.21 = num/sum(admits_lag.21)*100, 
  hosp_case = sum(admits_lag.21)/sum(cases_avg),
  hosp_case1 = sum(admits_lag.14)/sum(cases_avg)) %>% ungroup() %>%
  mutate(min_omi = cfr == min(cfr, na.rm = T),
         max_omi = cfr == max(cfr, na.rm = T))

# maximum
us2 %>% filter(max_omi) %>% mutate(ymd_shift = ymd + 21)

# minimum
us2 %>% filter(min_omi) %>% mutate(ymd_shift = ymd + 21)

# current
us2 %>% filter(ymd=="2022-03-15") %>% mutate(ymd_shift = ymd + 21)

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
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + ylim(0, 25) +
  #scale_y_continuous(breaks = c(0,.5, 1,2,3)) +
  scale_color_brewer(name = "", palette = "Set1") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) +
  labs(x = "", y = "", title = "Lagged COVID-19 hospital admission-fatality ratio (%)") 
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
  
