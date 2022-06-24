#### SETUP ####
library(here)
source(here("global_options.R"))

# states
h = read.csv(here("0 - Data", "hosps.csv")) %>% group_by(state) %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")-1) %>%
  arrange(date) %>%
  mutate(admits_confirmed = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         admits_suspected = previous_day_admission_adult_covid_suspected + previous_day_admission_pediatric_covid_suspected,
         admits = admits_confirmed + admits_suspected,
         admits_avg = rollmean(admits, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         admits_avg = ifelse(is.na(admits_avg), 0, admits_avg),
         admits_confirmed_avg = rollmean(admits_confirmed, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         admits_suspected_avg = rollmean(admits_suspected, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         hosped_avg = rollmean(total_adult_patients_hospitalized_confirmed_covid + total_pediatric_patients_hospitalized_confirmed_covid, k = 7, align = "right", na.pad = T, na.rm = T),
         perc_covid = rollmean(percent_of_inpatients_with_covid, k = 7, align = "right", na.pad = TRUE, na.rm = T))

# counties
h2 = read.csv(here("0 - Data", "hosps_county.csv")) %>% 
  mutate(fips = as.numeric(fips_code)) %>%
  mutate(date = as.Date(collection_week, format = "%Y/%m/%d")+5,
         previous_day_admission_adult_covid_confirmed_7_day_sum = ifelse(previous_day_admission_adult_covid_confirmed_7_day_sum < 0, 2, previous_day_admission_adult_covid_confirmed_7_day_sum),
         previous_day_admission_pediatric_covid_confirmed_7_day_sum = ifelse(previous_day_admission_pediatric_covid_confirmed_7_day_sum < 0, .5, previous_day_admission_pediatric_covid_confirmed_7_day_sum),
         previous_day_admission_adult_covid_suspected_7_day_sum = ifelse(previous_day_admission_adult_covid_suspected_7_day_sum < 0, NA, previous_day_admission_adult_covid_suspected_7_day_sum),
         previous_day_admission_pediatric_covid_suspected_7_day_sum = ifelse(previous_day_admission_pediatric_covid_suspected_7_day_sum < 0, 1, previous_day_admission_pediatric_covid_suspected_7_day_sum),
         inpatient_beds_used_covid_7_day_sum = ifelse(inpatient_beds_used_covid_7_day_sum < 0, 1, inpatient_beds_used_covid_7_day_sum),
         inpatient_beds_7_day_sum = ifelse(inpatient_beds_7_day_sum < 0, NA, inpatient_beds_7_day_sum)) %>%
  group_by(state, date) %>%
  summarize(admits_confirmed = sum(previous_day_admission_adult_covid_confirmed_7_day_sum, na.rm = T) + sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum, na.rm = T),
            admits_suspected = sum(previous_day_admission_adult_covid_suspected_7_day_sum, na.rm = T) + sum(previous_day_admission_pediatric_covid_suspected_7_day_sum, na.rm = T),
            inpt_beds_covid = sum(inpatient_beds_used_covid_7_day_sum, na.rm = T),
            inpt_beds = sum(inpatient_beds_7_day_sum, na.rm = T)) %>%
  mutate(admits = admits_confirmed + admits_suspected,
         admits_avg = admits/7,
         admits_confirmed_avg = admits_confirmed/7,
         admits_confirmed_avg = ifelse(is.na(admits_confirmed_avg), 0, admits_confirmed_avg),
         admits_suspected_avg = admits_suspected/7,
         perc_covid = inpt_beds_covid/7/inpt_beds,
         perc_covid = ifelse(perc_covid=="Inf", 0, perc_covid))

h_comb = h %>% mutate(id = "Original") %>% bind_rows(h2 %>% mutate(id = "Roll-up")) 
h_join = h %>% left_join(h2, c("state"="state", "date"="date")) %>% filter(!is.na(admits_confirmed_avg.y))
cor(h_join$admits_confirmed.x, h_join$admits_confirmed.y, use = "pairwise.complete.obs")
cor(h_join$perc_covid.x, h_join$perc_covid.y, use = "pairwise.complete.obs")

ggplot(h_comb %>% filter(state=="CT"), aes(x = date, y = admits_confirmed_avg, group = id, col = id)) + 
  geom_point(size = 1, alpha = .6, pch = 16)
