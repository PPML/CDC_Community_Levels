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
  filter(ymd>="2021-11-01") %>%
  summarize(
  num = sum(deaths_lag.21), denom = sum(cases_avg),
  cfr = num/denom*100,
  cfr.17 = sum(deaths_lag.17)/denom*100) %>% ungroup() %>%
  mutate(min_omi = cfr == min(cfr, na.rm = T),
         max_omi = cfr == max(cfr, na.rm = T))

# maximum
us2 %>% filter(max_omi) %>% mutate(ymd_shift = ymd + 21)

# minimum
us2 %>% filter(min_omi) %>% mutate(ymd_shift = ymd + 21)

# current
us2 %>% filter(ymd=="2022-02-14") %>% mutate(ymd_shift = ymd + 21)

