#### SETUP ####
library(here)
source(here("global_options.R"))

#### DATA ####
load(here("0 - Data", "combined_data_county.RData"))

#### PAPER PLOTS ####

# Figure 1 
dg = df %>% 
  # start in June
  filter(date >= "2021-06-01") %>% 
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Friday") %>%
  
  # set up order
  group_by(fips) %>%
  arrange(fips, date) %>%
  
  # define episode start (must last at least 2 weeks)
  mutate(trigger_on = cdc_flag & !lag(cdc_flag,1) & lead(cdc_flag,1)) %>%
  # define variables for plotting episodes
  group_by(fips) %>% mutate(grp = lag(cdc_flag,1)!=cdc_flag | is.na(lag(cdc_flag,1)),
                             n = n(),
                             epoch = sapply(1:n, function(a) sum(grp[1:a]))) %>%
  # calculate episode duration
  group_by(fips, epoch) %>% mutate(last = max(ymd[cdc_flag]), time = as.numeric(last-ymd)/7 +1,
                                   max = max(deaths_21_lag_100k[cdc_flag], na.rm = T)*7,
                                   label = paste(CTYNAME, ", ", ABBR, sep = ""))

# Table S1
# pull CDC indicators and lagged 21 day outcomes
d_out = dg %>% ungroup() %>% mutate(deaths_weekly = deaths_21_lag_100k*7,
                                    admits_weekly = admits_confirmed_100K*7,
                                    cases_weekly = round(cases_avg_per_100k*7),
                                    perc_covid_100 = perc_covid*100,
                                    trigger = ifelse(check_bound, "Cases", "Hosps")) %>%
  filter(trigger_on) %>%
  dplyr::select(ymd, fips, state, time, cases_weekly, admits_weekly, POPESTIMATE2019, perc_covid_100, deaths_weekly, trigger, max, check_bound, check_bound2, label)

# mortality 21-days later
d_out %>% summarize(mean = mean(deaths_weekly, na.rm = T),
                    median = median(deaths_weekly, na.rm = T),
                    chk = sum(is.na(deaths_weekly)))

# mortality 21-days later by epoch
d_out %>% mutate(epoch = ifelse(ymd>="2021-11-01", "Omicron", "Delta")) %>%
  group_by(epoch) %>% summarize(n = n(),
                                mean = mean(deaths_weekly, na.rm = T),
                                weighted.mean = weighted.mean(deaths_weekly, na.rm = T, w = POPESTIMATE2019),
                                median = median(deaths_weekly, na.rm = T),
                                q25 = quantile(deaths_weekly, .25, na.rm = T),
                                q75 = quantile(deaths_weekly, .75, na.rm = T),
                                #median.weighted = weighted.median(deaths_weekly, na.rm = T, w = POPESTIMATE2019),
                                #chk = sum(is.na(deaths_weekly))
                                )

# make plot
dg = dg %>% arrange(county_rank)
ranks = sort(unique(dg$county_rank))[1:54]
counties = unique(dg$label[dg$county_rank%in%ranks])

dh = dg %>% filter(county_rank %in% ranks) %>%
  arrange(POPESTIMATE2019) %>%
  mutate(label = factor(label, counties))

plot1 = ggplot(dh, aes(x = ymd + 21, y = deaths_21_lag_100k*7)) +
  geom_line(col = "grey", lwd = .3) + 
  geom_line(data = dh %>% filter(cdc_flag), 
            aes(x = ymd + 21, y = deaths_21_lag_100k*7, group = paste(state, epoch)), col = "navy", lwd = .3) +
  facet_wrap(.~label, ncol = 6) + 
  geom_point(data = dh %>% filter(trigger_on), pch = 16, col = "navy",
             aes(x = ymd + 21, y = deaths_21_lag_100k*7)) + 
  geom_text(data = dh %>% filter(trigger_on), aes(x = ymd-7 + 21, y = deaths_21_lag_100k*7 + 4,
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
plot1


ggsave(here("2 - Figures", "cdc_plot_deaths_county.png"), plot = plot1, width = 10, height = 11)



