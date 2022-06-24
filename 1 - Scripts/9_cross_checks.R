#****************************** CHECK COMM LEVELS *****************************#
#*                                                                            *#
#*                                                                            *#
#* Check out calculations against estimated CDC levels                        *#
#******************************************************************************#

#### SETUP ####
library(here)
source(here("global_options.R"))

# our data
d = read.csv(here("0 - Data", "combined_data_county.csv")) %>%
  mutate(ymd = as.Date(ymd, format = "%Y-%m-%d"), fips = as.numeric(fips)) %>%
  filter(dotw=="Wednesday")

# community levels
c = read.csv(here("0 - Data", "United_States_COVID-19_Community_Levels_by_County.csv")) %>%
  mutate(ymd = as.Date(date_updated, "%m/%d/%y")-1, fips = as.numeric(county_fips)) %>%
  filter(ymd <= "2022-06-03")

# compare datasets on fips
length(unique(d$fips))
length(unique(c$fips))
unique(c$fips[!c$fips%in%d$fips])
unique(d$fips[!d$fips%in%c$fips])

# compare datasets on dates
length(unique(d$ymd))
length(unique(c$ymd))
unique(c$ymd[!c$ymd%in%d$ymd])
unique(d$ymd[!d$ymd%in%c$ymd])

# merge datasets
e = c %>% inner_join(d, c("fips" = "fips", "ymd" = "ymd")) %>%
  mutate(cdc_high = covid.19_community_level=="High",
         admits_confirmed_100K=admits_confirmed_100K*7, cases_avg_per_100k = cases_avg_per_100k*7, perc_covid = perc_covid*100,
         flag_alt1_us = admits_confirmed_100K > 20,# | perc_covid > 15,#
         flag_alt1_cdc = covid_hospital_admissions_per_100k > 20,# | covid_inpatient_bed_utilization > 15,
         flag_alt2_us = admits_confirmed_100K > 10,# | perc_covid > 10,
         flag_alt2_cdc = covid_hospital_admissions_per_100k > 10)# | covid_inpatient_bed_utilization > 10)
table(e$cdc_high, e$cdc_flag)
table(e$flag_alt1_us, e$flag_alt1_cdc)
table(e$flag_alt2_us, e$flag_alt2_cdc)


cor(e$admits_confirmed_100K, e$covid_hospital_admissions_per_100k, use = "pairwise.complete.obs") 
cor(e$perc_covid, e$covid_inpatient_bed_utilization, use = "pairwise.complete.obs") 
cor(e$covid_hospital_admissions_per_100k, e$admits_confirmed_100K, use = "pairwise.complete.obs") 

chk1 = e %>% filter(cdc_flag & !cdc_high) %>% 
  dplyr::select(fips, ymd, 
                covid_cases_per_100k, cases_avg_per_100k,
                covid_inpatient_bed_utilization, perc_covid,
                covid_hospital_admissions_per_100k, admits_100K) 

chk2 = e %>% filter(!cdc_flag & cdc_high) %>% 
  mutate(admits_100K=admits_100K*7, cases_avg_per_100k = cases_avg_per_100k*7, perc_covid = perc_covid*100) %>%
  dplyr::select(fips, ymd, 
                covid_cases_per_100k, cases_avg_per_100k,
                covid_inpatient_bed_utilization, perc_covid,
                covid_hospital_admissions_per_100k, admits_100K) 

chk3 = e %>%
  dplyr::select(fips, ymd, county_population,
                covid_cases_per_100k, cases_avg_per_100k,
                covid_inpatient_bed_utilization, perc_covid,
                covid_hospital_admissions_per_100k, admits_100K) 


ggplot(e %>% filter(county_population > 1e6) %>% gather(var, value, covid_cases_per_100k, cases_avg_per_100k), 
       aes(x = ymd, y = value, group = paste(fips, var), col = var)) + geom_line() + 
  facet_wrap(.~fips, scales = "free")

ggplot(e %>% filter(county_population > 1e6) %>% gather(var, value, covid_hospital_admissions_per_100k, admits_confirmed_100K), 
       aes(x = ymd, y = value, group = paste(fips, var), col = var)) + geom_line() + 
  facet_wrap(.~fips, scales = "free")

ggplot(e %>% filter(county_population > 1e6) %>% gather(var, value, perc_covid, covid_inpatient_bed_utilization), 
       aes(x = ymd, y = value, group = paste(fips, var), col = var)) + geom_line() + 
  facet_wrap(.~fips, scales = "free")

ggplot(e , 
       aes(x = covid_hospital_admissions_per_100k, y = admits_confirmed_100K)) + 
  geom_point(alpha = .1) + ylim(0, 50) + xlim(0, 50)

ggplot(e %>% filter(county_population > 1e6) %>% gather(var, value, covid_hospital_admissions_per_100k, admits_confirmed_100K) %>%
         mutate(flag = ifelse(var=="covid_hospital_admissions_per_100k", cdc_high, cdc_flag)), 
       aes(x = ymd, y = value, group = paste(fips, var, flag), col = flag, pch = var)) + 
  geom_point(size = 1) + 
  scale_shape_manual(values = c(1,16)) + 
  facet_wrap(.~fips, scales = "free")

e %>% filter(county_population > 1e6) %>% summarize(cor(cdc_flag, cdc_high, use = "pairwise.complete"))


