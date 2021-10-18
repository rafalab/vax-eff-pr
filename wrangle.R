##############
#### WRANGLING
##############
source("init.R")
source("funcs.R")

analysis_last_day <- last_day

## must run init.R first
load("rdas/population-tabs.rda")
load("rdas/dat_cases_vax.rda")
load("rdas/dat_vax.rda")

### VACCINE COUNTS

# Creates all variable combinations that will be used

all_combs <- expand_grid(date = all_dates$date, 
                         gender = c("M","F"), 
                         ageRange = factor(age_levels, levels = age_levels),
                         manu = manu_levels[manu_levels != "UNV"])

## Counts the number of completely vaccinated individuals (vax) by date, age range
## gender and vaccine manufacturer

daily_counts_vax_age_gender_manu <- dat_vax %>%
  filter(!is.na(gender_1) & !is.na(ageRange_1) &
           !is.na(date_2) & !is.na(manu_1)) %>%
  filter(manu_1 %in% c("PFR", "MOD", "JSN"),
         gender_1 %in% c("F", "M")) %>%
  mutate(date = pmax(date_2 + days(wait), first_day), 
         ageRange = ageRange_1, 
         manu = manu_1, 
         gender = gender_1) %>%
  group_by(date, ageRange, gender, manu) %>%
  summarize(vax = n(), .groups = "drop") %>%
  ungroup() %>%
  right_join(all_combs, by = c("date", "gender", "ageRange", "manu")) %>%
  mutate(vax = replace_na(vax, 0)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange, gender, manu)

## Counts the number of one dose vaccinated individuals by date, age range
## gender and vaccine manufacturer

daily_counts_onedose_age_gender_manu <- dat_vax %>%
  filter(!is.na(gender_1) & !is.na(ageRange_1) &
           !is.na(manu_1)) %>%
  filter(manu_1 %in% c("PFR", "MOD", "JSN"),
         gender_1 %in% c("F", "M")) %>%
  mutate(date = date_1, 
         ageRange = ageRange_1, 
         manu = manu_1, 
         gender = gender_1) %>%
  group_by(date, ageRange, gender, manu) %>%
  summarize(vax = n(), .groups = "drop") %>%
  ungroup() %>%
  right_join(all_combs, by = c("date", "gender", "ageRange", "manu")) %>%
  mutate(vax = replace_na(vax, 0)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange, gender, manu)

## Cumulative sum of completely vaccinated individuals by age range, gender and
## vaccine manufacturer

counts_vax_age_gender_manu <-
  daily_counts_vax_age_gender_manu %>%
  group_by(ageRange, gender, manu) %>%
  mutate(vax = cumsum(vax)) %>%
  ungroup()

## Cumulative sum of one dose vaccinated individuals by age range, gender and
## vaccine manufacturer 

counts_onedose_age_gender_manu <-
  daily_counts_onedose_age_gender_manu %>%
  group_by(ageRange, gender, manu) %>%
  mutate(vax = cumsum(vax)) %>%
  ungroup()

## Population of unvaccinated individuals by age Range, date and gende

unvax <- counts_onedose_age_gender_manu %>%
  group_by(date, ageRange, gender) %>%
  summarize(total = sum(vax), .groups = "drop") %>%
  ungroup() %>%
  left_join(pop_by_age_gender, by = c("ageRange", "gender")) %>%
  mutate(poblacion = poblacion - total, manu = "UNV") %>%
  select(-total)

## Population numbers by date, age range, gender, vaccine manufacture (or status)

pop_age_gender_manu <- counts_vax_age_gender_manu %>%
  rename(poblacion = vax) %>%
  bind_rows(unvax) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(date, ageRange,  manu, gender)

rm(unvax) # removes unvax from environment; no longer needed

## Individual case level information with number of days between case detection and 
## vaccination status (VAX, PAR, UNV)

dat <- dat_cases_vax %>% 
  filter(!is.na(ageRange) & !is.na(gender)) %>%
  replace_na(list(manu = "UNV")) %>%
  filter(date <= analysis_last_day &
           gender %in% c("F", "M") & 
           manu %in% c("UNV", "PFR", "MOD", "JSN")) %>%
  mutate(status = case_when(date > dose_2 + days(wait) ~ "VAX",
                            date > dose_1 ~ "PAR",
                            TRUE ~ "UNV")) %>%
  filter(status != "PAR") %>%
  mutate(manu = ifelse(status == "UNV", "UNV", manu)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  mutate(day = as.numeric(date) - as.numeric(dose_2+days(wait)))

## Counts the number of cases, hosp and deaths by vaccine manufacturer and status, 
## age range, gender

counts_cases <- dat %>%
  group_by(date, ageRange, gender, manu) %>% 
  summarize(cases = n(), hosp = sum(hosp), deaths = sum(death), .groups = "drop") %>% 
  filter(date >= first_day & date <= analysis_last_day) %>%
  complete(date, ageRange, gender, manu,  fill = list(cases = 0, hosp = 0, deaths = 0)) %>%
  left_join(pop_age_gender_manu, by = c("date", "ageRange", "gender", "manu")) %>%
  mutate(delta = factor(if_else(date < delta_date, "before", "after"), 
                        levels = c("before","after")))

### WANING
ndays <- as.numeric(analysis_last_day - first_day)
  
the_pop <- pop_by_age_gender %>% 
  rename(total_pop = poblacion)

## Counts the number of unvaccinated population by date, age range and gender

pop_unvax <- daily_counts_onedose_age_gender_manu %>%
  filter(date >= first_day & date <= analysis_last_day) %>%
  group_by(date, ageRange, gender) %>%
  summarize(poblacion = sum(vax), .groups = "drop") %>%
  left_join(the_pop, by = c("ageRange", "gender")) %>%
  group_by(ageRange, gender) %>%
  arrange(date) %>%
  mutate(poblacion = total_pop - cumsum(poblacion)) %>%
  ungroup() %>%
  select(-total_pop)

  ## check
  # pop_unvax %>% ggplot(aes(date, poblacion, color = ageRange,lty=gender)) + geom_line()

unvax <- dat %>% 
  filter(manu == "UNV") %>% 
  group_by(date, ageRange, gender) %>%
  summarize(cases = n(), hosp = sum(!is.na(date_hosp)), 
            deaths = sum(death), .groups = "drop")  %>%
  right_join(pop_unvax, by = c("date", "ageRange", "gender")) %>%
  replace_na(list(cases = 0, hosp = 0, deaths = 0)) 
  ## check
  #unvax %>% ggplot(aes(date, cases, color = ageRange, lty=gender)) + geom_line()
  #unvax %>% ggplot(aes(date, poblacion , color = ageRange,lty=gender)) + geom_line()
  #unvax %>% ggplot(aes(date, cases/poblacion * 10^5, color = ageRange,lty=gender)) + geom_line()

## Now the vaccinated
## pop_vax will have number of people vaccinated `day` days after `date`.
## we keep only day/date combos with more than 1 person vaccinated
add_days <- function(tmp, n){
  map_df(1:n, function(n){
    data.frame(date = tmp$date, day = n, poblacion = lag(tmp$poblacion, n))
  }) %>%  
    filter(!is.na(poblacion)) %>%
    filter(poblacion > 0)
}

pop_vax <- daily_counts_vax_age_gender_manu %>%
  filter(date >= first_day & date <= analysis_last_day) %>%
  group_by(date, ageRange, gender, manu) %>%
  summarize(poblacion = sum(vax), .groups = "drop") %>%
  group_by(manu, ageRange, gender) %>%
  do(add_days(., n = ndays))

## Now we will add the number of cases for each date/day combination with at least one vaccinated

waning <- dat %>% 
  filter(day>0 & day < ndays) %>%
  group_by(manu, ageRange, gender, date, day) %>% 
  summarize(cases = n(), hosp = sum(!is.na(date_hosp)), 
            deaths = sum(death), .groups = "drop")  %>%  
  right_join(pop_vax, by = c("manu", "ageRange", "gender", "date", "day")) %>% # adding vaccinated population
  replace_na(list(cases = 0, hosp = 0, deaths = 0)) %>%
  mutate(delta = factor(if_else(date - day < delta_date, "before", "after"), levels = c("before", "after"))) %>%
  arrange(manu, ageRange, day, date) %>%
  mutate(manu = factor(manu, levels = manu_levels))

