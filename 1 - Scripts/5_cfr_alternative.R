#### SETUP ####
library(here)
source(here("global_options.R"))
source(here("1 - Scripts", "2_in_text_numbers.R"))

#### DATA ####

# age-adjusted
df = read.csv(here("0 - Data", "cases_by_vax.csv")) %>% 
  filter(Vaccine.product == "all_types" &
           Age.group == "all_ages_adj") %>%
  mutate(date = MMWRweek2Date(Year, week)) %>% 
  group_by(outcome) %>%
  group_by(Age.group, Vaccine.product, outcome) %>%
  mutate(rel_rate = Vaccinated.with.outcome/(Vaccinated.with.outcome+Unvaccinated.with.outcome),
         lead3_rank = lead(date, 3),
         lead3_vax = lead(Vaccinated.with.outcome, 3),
         lead3_unvax = lead(Unvaccinated.with.outcome, 3),
         lead3_vax2 = lead(Age.adjusted.vax.IR, 3),
         lead3_unvax2 = lead(Age.adjusted.unvax.IR, 3)) %>%
  group_by(date) %>%
  mutate(len = sum(outcome=="death"),
         deaths_vax = ifelse(len > 0, lead3_vax2[outcome=="death"], NA),
         deaths_unvax = ifelse(len > 0, lead3_unvax2[outcome=="death"], NA),
         deaths_vax.crude = ifelse(len > 0, lead3_vax[outcome=="death"], NA),
         deaths_unvax.crude = ifelse(len > 0, lead3_unvax[outcome=="death"], NA),
         cfr_vax = deaths_vax/Age.adjusted.vax.IR,
         cfr_unvax = deaths_unvax/Age.adjusted.unvax.IR,
         cfr_vax.crude = deaths_vax.crude/Vaccinated.with.outcome,
         cfr_unvax.crude = deaths_unvax.crude/Unvaccinated.with.outcome,
         cfr_vax_no_lag = ifelse(len > 0, Age.adjusted.vax.IR[outcome=="death"], NA)/Age.adjusted.vax.IR,
         cfr_unvax_no_lag = ifelse(len > 0, Age.adjusted.unvax.IR[outcome=="death"], NA)/Age.adjusted.unvax.IR)
  

ggplot(df %>% 
         gather(var, value, Crude.vax.IR, Crude.unvax.IR), 
       aes(x = date, y = value, group = var, col = var)) + 
  geom_line() + theme_minimal() + labs(x = "", y = "Rate per 100K") + 
  scale_color_discrete(name = "") + facet_wrap(.~outcome, scales = "free")

ggplot(df,
       aes(x = date, y = rel_rate)) + 
  geom_line() + theme_minimal() + labs(x = "", y = "Rate per 100K") + 
  scale_color_discrete(name = "") + facet_wrap(.~outcome, scales = "free")

ggplot(df %>% filter(date>="2021-07-22") %>% filter(outcome=="death") %>%
         gather(var, value, Age.adjusted.vax.IR, Age.adjusted.unvax.IR), 
       aes(x = date, y = value, group = var, col = var)) + 
  geom_line() + theme_minimal() + labs(x = "", y = "Rate per 100K") + 
  scale_color_discrete(name = "") + facet_wrap(.~outcome, scales = "free")

ggplot(df %>%
         gather(var, value, cfr_unvax, cfr_vax, cfr_unvax.crude, cfr_vax.crude) %>% 
         filter(outcome=="case"),
       aes(x = date, y = value, group = var, col = grepl("unvax", var), lty = grepl("crude", var))) + 
  geom_line() + theme_minimal() + labs(x = "Time", y = "CFR") + 
  scale_color_discrete()

ggplot(df %>%
         gather(var, value, cfr_vax_no_lag, cfr_unvax_no_lag) %>% filter(outcome=="case"),
       aes(x = date, y = value, group = var, col = var)) + 
  geom_line() + theme_minimal() + labs(x = "Time", y = "CFR") + 
  scale_color_discrete(name = "")

ggplot(df %>% filter(outcome=="case"), aes(x = rank, y = value, group = var, col = var)) + 
  geom_line() + theme_minimal() + labs(x = "Time", y = "CFR") + 
  scale_color_discrete(name = "")


# not age-adjusted
df2 = read.csv(here("0 - Data", "cases_by_vax.csv")) %>% 
  filter(Vaccine.product == "all_types" &
           Age.group != "all_ages_adj") %>%
  group_by(MMWR.week, outcome) %>%
  summarize(Vaccinated.with.outcome = sum(Vaccinated.with.outcome),
            Unvaccinated.with.outcome = sum(Unvaccinated.with.outcome)) %>%
  group_by(outcome) %>%
  mutate(rank = rank(MMWR.week)) %>%
  group_by(outcome) %>%
  mutate(lead3_vax = lead(Vaccinated.with.outcome, 3),
         lead3_unvax = lead(Unvaccinated.with.outcome, 3)) %>%
  group_by(MMWR.week) %>%
  mutate(len = sum(outcome=="death"),
         deaths_vax = ifelse(len > 0, lead3_vax[outcome=="death"], NA),
         deaths_unvax = ifelse(len > 0, lead3_unvax[outcome=="death"], NA),
         cfr_vax = deaths_vax/Vaccinated.with.outcome,
         cfr_unvax = deaths_unvax/Unvaccinated.with.outcome) %>%
  gather(var, value, cfr_unvax, cfr_vax) %>%
  filter(outcome=="case")

ggplot(df2, aes(x = rank, y = value, group = var, col = var)) + 
  geom_line() + theme_minimal() + labs(x = "Time", y = "CFR") + 
  scale_color_discrete(name = "")
