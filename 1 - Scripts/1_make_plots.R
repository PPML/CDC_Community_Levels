#### SETUP ####
library(here)
source(here("global_options.R"))

#### DATA ####
load(here("0 - Data", "combined_data.RData"))

#### PAPER PLOTS ####

# Figure 1 
dg = df %>% 
  # start in June
  filter(date >= "2021-06-01") %>% 
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Wednesday") %>%
  # define episode start (must last at least 2 weeks)
  mutate(trigger_on = cdc_flag & !lag(cdc_flag,1) & lead(cdc_flag,1)) %>%
  # define variables for plotting episodes
  group_by(state) %>% mutate(grp = lag(cdc_flag,1)!=cdc_flag | is.na(lag(cdc_flag,1)),
                             n = n(),
                             epoch = sapply(1:n, function(a) sum(grp[1:a]))) %>%
  # calculate episode duration
  group_by(state, epoch) %>% mutate(last = max(ymd[cdc_flag]), time = as.numeric(last-ymd)/7 +1,
                                    max = max(deaths_21_lag_100k[cdc_flag], na.rm = T)*7)

# create figure 1
plot1 = ggplot(dg, aes(x = ymd + 21, y = deaths_21_lag_100k*7)) +
  geom_line(col = "grey", lwd = .3) + 
  geom_line(data = dg %>% filter(cdc_flag), 
            aes(x = ymd + 21, y = deaths_21_lag_100k*7, group = paste(state, epoch)), col = "navy", lwd = .3) +
  facet_wrap(.~state, ncol = 6) + 
  geom_point(data = dg %>% filter(trigger_on), pch = 16, col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7)) + 
  geom_text(data = dg %>% filter(trigger_on), aes(x = ymd-7 + 21, y = deaths_21_lag_100k*7 + 4,
                                                  label = format(ifelse(deaths_21_lag_100k*7 < 10,
                                                                        round(deaths_21_lag_100k*7,1),
                                                                        round(deaths_21_lag_100k*7)), nsmall = 1)),
            size = 2) + 
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  scale_color_brewer(guide = "none", name = "", palette = "Set1") + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 9, vjust = -1.3)) + 
  labs(x = "", y = "Average weekly deaths per 100K population") + 
  geom_hline(yintercept = 0.9, lty = 3) 
ggsave(here("2 - Figures", "cdc_plot_deaths.png"), plot = plot1, width = 8, height = 11)

# create figure 1
plot1 = ggplot(dg, aes(x = ymd + 21, y = deaths_21_lag_100k*7)) +
  geom_line(col = "grey", lwd = .3) + 
  geom_line(data = dg %>% filter(cdc_flag), 
            aes(x = ymd + 21, y = deaths_21_lag_100k*7, group = paste(state, epoch)), col = "navy", lwd = .3) +
  facet_wrap(.~state, ncol = 8) + 
  geom_point(data = dg %>% filter(trigger_on), pch = 16, col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7)) + 
  geom_text(data = dg %>% filter(trigger_on), aes(x = ymd-7 + 21, y = deaths_21_lag_100k*7 + 4,
                                                  label = format(ifelse(deaths_21_lag_100k*7 < 10,
                                                                        round(deaths_21_lag_100k*7,1),
                                                                        round(deaths_21_lag_100k*7)), nsmall = 1)),
            size = 2) + 
  scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y") +
  scale_color_brewer(guide = "none", name = "", palette = "Set1") + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 9, vjust = -1.3)) + 
  labs(x = "", y = "Average weekly deaths per 100K population") + 
  geom_hline(yintercept = 0.9, lty = 3) 
ggsave(here("2 - Figures", "cdc_plot_deaths2.png"), plot = plot1, width = 11, height = 8)

# Table S1
# pull CDC indicators and lagged 21 day outcomes
d_out = dg %>% ungroup() %>% mutate(deaths_weekly = deaths_21_lag_100k*7,
                                    admits_weekly = admits_confirmed_100K*7,
                                    cases_weekly = round(cases_avg_per_100k*7),
                                    perc_covid_100 = perc_covid*100,
                                    trigger = ifelse(check_bound, "Cases", "Hosps")) %>%
  filter(trigger_on) %>%
  dplyr::select(ymd, state, time, cases_weekly, admits_weekly, perc_covid_100, deaths_weekly, trigger, max, check_bound, check_bound2) %>%
  rename("Start date" = 1, "State" = 2, "Duration of 'high' episode (weeks)" = 3, "Weekly cases per 100K" = 4,
         "Weekly hospital admissions per 100K" = 5, "Percentage of inpatient beds occupied by COVID-19 patients" = 6,
         "Weekly deaths per 100K 21 days after start" = 7, "Binding indicator" = 8)
write.csv(d_out, file = here("2 - Figures", "table_s1.csv"))


# Figure 2
us = df %>% 
  # start in June
  filter(ymd >= "2021-06-01") %>% 
  # calculate state CFR
  group_by(ymd, state, dotw, REGION) %>% 
  summarize(deaths_lag.21 = sum(deaths_21_lag),
            deaths_lag.17 = sum(deaths_17_lag),
            admits_lag.14 = sum(admits_7_lag),
            admits_lag.21 = sum(admits_confirmed_avg),
            hosped_avg = sum(hosped_avg),
            cases_avg = sum(cases_avg),
            cfr = deaths_lag.21/cases_avg*100,
            cfr.17 = deaths_lag.17/cases_avg*100) %>% 
  # report 1x/week to reduce noise
  #filter(dotw == "Monday") %>% 
  # calculate regional CFR
  group_by(ymd, REGION) %>% 
  mutate(num_reg = sum(deaths_lag.21),
         num_reg.17 = sum(deaths_lag.17),
         denom_reg = sum(cases_avg),
         hosped_avg = sum(hosped_avg),
         cfr_region = num_reg/denom_reg*100,
         cfr_region.17 = num_reg.17/denom_reg*100,
         hosp_case1 = admits_lag.21/cases_avg,
         hosp_case2 = admits_lag.14/cases_avg,
         hosp_case_reg = sum(admits_lag.21)/sum(cases_avg),
         region = case_when(REGION=="1"~"Northeast", REGION=="2"~"Midwest", REGION=="3"~"South", REGION=="4"~"West"),
         region = factor(region, levels = c("Northeast", "Midwest", "South", "West")))

# create figure 2
plot2 = ggplot(us %>% filter(ymd <= "2022-03-01" & dotw == "Tuesday"), aes(x = ymd, y = cfr, group = state)) +
  geom_line(alpha = .15) + facet_grid(.~region) + 
  geom_line(data = us %>% filter(ymd <= "2022-03-01" & dotw == "Tuesday"), aes(x = ymd, y = cfr_region), col = "#000080") + 
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) + 
  geom_hline(yintercept = .45, col = "black", lty = 3) + 
  labs(x = "", y = "21-day lagged CFR (%)", title = "") +
  ylim(0, 4) 
ggsave(here("2 - Figures", "cfr_regional_21.png"), plot = plot2, width = 8, height = 4)

# create figure s1
plot_s1 = ggplot(us %>% filter(ymd <= "2022-02-15" & dotw == "Tuesday"), aes(x = ymd, y = cfr, group = state)) +
  geom_line(alpha = .15) + facet_grid(.~region) + 
  geom_line(data = us, aes(x = ymd, y = cfr_region), col = "#000080") + 
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x=element_text(size = 7, angle=60, hjust=1),
        strip.text=element_text(size = 10),
        plot.title = element_text(face = "bold")) + 
  geom_hline(yintercept = 0.45, col = "black", lty = 3) + 
  labs(x = "", y = "17-day lagged CFR (%)", title = "") +
  ylim(0, 4) 

ggsave(here("2 - Figures", "cfr_regional_17.png"), plot = plot_s1, width = 8, height = 4)


df = data.frame(x = 1:30) %>% mutate(y = 1/(1+exp(-.75*x+10)))
log = ggplot(df, aes(x = x, y = y)) + geom_line() + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(), 
                                                                      axis.line = element_blank(),
                                                                      axis.text.x=element_text(size = 7, angle=60, hjust=1),
                                                                      strip.text=element_text(size = 9, vjust = -1.3)) + labs(x = "Time", y = expression('q'[t]))
ggsave(here("2 - Figures", "logistic_curve2.png"), plot = log, width = 4, height = 2)


