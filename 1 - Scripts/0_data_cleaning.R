#### SETUP ####
library(tidyverse)
library(covidcast)
library(lubridate)
library(scales)
library(zoo)
library(TTR)
library(RColorBrewer)
library(here)

#### STATE POPULATION DATA ####
data(state_census)

#### HOSPITALIZATIONS ####
h = read.csv(here("0 - Data", "hosps.csv")) %>% group_by(state) %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  arrange(date) %>%
  mutate(admits_confirmed = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
         admits_suspected = previous_day_admission_adult_covid_suspected + previous_day_admission_pediatric_covid_suspected,
         admits = admits_confirmed + admits_suspected,
         admits_avg = rollmean(admits, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         admits_confirmed_avg = rollmean(admits_confirmed, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         admits_suspected_avg = rollmean(admits_suspected, k = 7, align = "right", na.pad = TRUE, na.rm = T),
         perc_covid = rollmean(percent_of_inpatients_with_covid, k = 7, align = "right", na.pad = TRUE, na.rm = T))

#### ANOMALOUS MD DATA ####
# dates of missing data
dates = seq(as.Date("2021-12-05"), as.Date("2021-12-19"), "days")

# pull in data from the state
m = read.csv(here("0 - Data", "MDCOVID19_CasesPer100KpopulationStatewide.csv")) %>%
  separate(ReportDate, into = c("Date", "Time"), sep = "\ ") %>%
  mutate(date = as.Date(Date, format = "%Y/%m/%d"),
         state = "Maryland") %>% filter(date %in% dates)

#### CASE DATA ####
df = read.csv(here("0 - Data", "us-states.csv")) %>% 
  
  # join to state data
  left_join(state_census, c("state"="NAME")) %>%
  filter(!is.na(POPESTIMATE2019)) %>%
  
  # group by state
  group_by(state) %>% 
  
  # format dates
  mutate(ymd = as.Date(date, format = "%Y-%m-%d"),
         year = year(ymd),
         week = epiweek(ymd),
         year_wk = paste(year, week, sep = "-")) %>% 
  
  arrange(ymd, .by_group = TRUE) %>%
  
  # filter out PR & Virgin Islands & arrange
  filter(!state%in%c("American Samoa", "Guam",
                     "Northern Mariana Islands", 
                     "Puerto Rico Commonwealth",
                     "U.S. Virgin Islands", 
                     "United States")) %>%
  arrange(state, ymd) %>%
  
  # join to hospital data
  left_join(h %>% dplyr::select(date, state, admits_avg,
                                admits_confirmed_avg, admits_suspected_avg, perc_covid), 
            c("ABBR"="state", "ymd"="date")) %>%
  mutate(admits_confirmed_100K = admits_confirmed_avg/POPESTIMATE2019*100000,
         admits_100K = admits_avg/POPESTIMATE2019*100000) %>%
  
  # link to MD
  left_join(m %>% dplyr::select(date, state, Statewide), 
            c("state" = "state", "ymd" = "date")) %>%
  
  # estimate CDC metrics
  mutate(
    
  # fix MD as needed
  cases_avg_per_100k = ifelse(is.na(Statewide), cases_avg_per_100k, Statewide),
    
  # remove NAs from bed percentages
  perc_covid = ifelse(is.na(perc_covid), 0, perc_covid),
  
  # define CDC "high"
  cdc_flag_1 = (cases_avg_per_100k > 200/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_2 = (cases_avg_per_100k < 200/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)), # under 200/100K 7-d
  cdc_flag = cdc_flag_1 | cdc_flag_2,
  
  cdc_flag_inc_susp = (cases_avg_per_100k > 200/7 & (admits_100K > 10/7 | perc_covid > .1)) |
    (cases_avg_per_100k < 200/7 & (admits_100K > 20/7 | perc_covid > .15)),

  # lagged deaths
  deaths_17_lag = lead(deaths_avg, 17), 
  deaths_21_lag = lead(deaths_avg, 21), 
  
  # lagged deaths per 100k
  deaths_12_lag_100k = lead(deaths_avg_per_100k, 12), 
  deaths_13_lag_100k = lead(deaths_avg_per_100k, 13), 
  deaths_14_lag_100k = lead(deaths_avg_per_100k, 14), 
  deaths_15_lag_100k = lead(deaths_avg_per_100k, 15), 
  deaths_16_lag_100k = lead(deaths_avg_per_100k, 16), 
  deaths_17_lag_100k = lead(deaths_avg_per_100k, 17), 
  deaths_18_lag_100k = lead(deaths_avg_per_100k, 18), 
  deaths_19_lag_100k = lead(deaths_avg_per_100k, 19), 
  deaths_20_lag_100k = lead(deaths_avg_per_100k, 20), 
  deaths_21_lag_100k = lead(deaths_avg_per_100k, 21), 
  deaths_22_lag_100k = lead(deaths_avg_per_100k, 22), 
  deaths_23_lag_100k = lead(deaths_avg_per_100k, 23), 
  deaths_24_lag_100k = lead(deaths_avg_per_100k, 24), 
  deaths_25_lag_100k = lead(deaths_avg_per_100k, 25), 
  deaths_26_lag_100k = lead(deaths_avg_per_100k, 26),
  
  # day of the week
  dotw = weekdays(ymd),
  
  # check completeness
  chk = paste(ymd, state))

k = table(df$chk)
k[k > 1]



#### save
write.csv(df, here("0 - Data", "combined_data.csv"))
save(df, file = here("0 - Data", "combined_data.RData"))




