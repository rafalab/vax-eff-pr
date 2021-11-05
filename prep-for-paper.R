total_cases <- prettyNum(sum(counts_cases$cases), big.mark = ",")

#counts_vax_age_gender_manu %>% group_by(date) %>% summarize(n=sum(vax)/pr_pop) %>% filter(date==make_date(2021, 07, 15)) %>% pull(n)
vax_status <- brewer.pal(9, "Set1")## colors for vaccination status

plot_manu_levels <- c("UNV", "MOD", "PFR", "JSN", "before", "after")
manu_labels <- c(UNV = "Unvaccinated", MOD = "mRNA-1273", PFR = "BNT162b2", JSN = "Ad26.COV2.S", 
                 before = paste("Before", format(delta_date, "%B %e, %Y")), after = paste("After", format(delta_date, "%B %e, %Y")))
manu_colors <- c(UNV = vax_status[1], MOD = "#00BFC4", PFR = "#C77CFF", JSN = "#7CAE00", before = hue_pal()(9)[[1]], after = hue_pal()(9)[[6]])

## rollout
rollout <- read_delim("data/vax_rollout.txt", "\t") %>%
  mutate(Date = format(as_date(Date), format="%b %e, %Y"))


## WANING
max_days <- expand.grid(ageRange = age_levels, manu = manu_levels[-1]) %>%
  mutate(age=get_age(ageRange)) %>%
  mutate(start = case_when(age>=75 & manu !="JSN" ~ make_date(2021, 1, 11),
                           age>=75 ~ make_date(2021, 3, 3),
                           age>=65 ~ make_date(2021, 3, 12),
                           age>=55 ~ make_date(2021, 3, 29),
                           age>=16 ~ make_date(2021, 4, 12),
                           TRUE ~ make_date(2021, 5, 12))) %>%
  mutate(start = case_when(manu=="MOD" ~ 28+14, 
                           manu=="PFR" ~ 21+14, 
                           manu=="JSN" ~ 14)+
           as.numeric(start)) %>% 
  mutate(max.day = as.numeric(analysis_last_day) - start) %>%
  select(-start) %>%
  filter(!(ageRange=="12-17" & manu != "PFR"))

the_max_day <- max_days %>% 
  filter(ageRange!="12-17") %>% 
  group_by(manu) %>%
  summarize(max.day = min(max.day))

### WANING FOR CASES
cases_exp <- compute_expected(unvax, outcome = "cases") %>%
  mutate(rate = fit / poblacion) %>%
  select(date, ageRange, gender, rate)

waning_dat <- waning %>% left_join(cases_exp, by = c("date", "ageRange", "gender")) %>%
  mutate(exp = rate * poblacion) %>% 
  group_by(delta, ageRange, manu, day) %>%
  summarize(obs = sum(cases), exp = sum(exp), .groups = "drop")

waning_fit <- waning_dat %>% 
  filter(ageRange!="12-17") %>%
  group_by(manu, day) %>%
  summarize(obs = sum(obs), exp = sum(exp), .groups = "drop") %>%
  left_join(the_max_day, by="manu") %>%
  group_by(manu) %>%
  do(fit_waning_model(.))
one_max_day <- min(the_max_day$max.day)

waning_tab <- waning_fit %>% 
  group_by(manu) %>%
  arrange(day) %>%
  filter(day < one_max_day) %>%
  summarize(days = unique(max.day),
            day = first(day), 
            max = first(fit),
            max.up=first(conf.high), 
            max.lo=first(conf.low),
            min = last(fit), 
            min.up=last(conf.high), 
            min.lo=last(conf.low)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(manu)

### WANING FOR HOSP
hosp_exp <- compute_expected(unvax, outcome = "hosp") %>%
  mutate(rate = fit / poblacion) %>%
  select(date, ageRange, gender, rate)

waning_hosp_dat <- waning %>% left_join(hosp_exp, by = c("date", "ageRange", "gender")) %>%
  mutate(exp = rate * poblacion) %>% 
  group_by(delta, ageRange, manu, day) %>%
  summarize(obs = sum(hosp), exp = sum(exp), .groups = "drop")

waning_hosp_fit <- waning_hosp_dat %>% 
  filter(ageRange!="12-17") %>%
  group_by(manu, day) %>%
  summarize(obs = sum(obs), exp = sum(exp), .groups = "drop") %>%
  left_join(the_max_day, by="manu") %>%
  group_by(manu) %>%
  do(fit_waning_model(.))

waning_hosp_tab <- waning_hosp_fit %>% 
  group_by(manu) %>%
  arrange(day) %>%
  filter(day < one_max_day) %>%
  summarize(days = unique(max.day),
            day = first(day), 
            max = first(fit),
            max.up=first(conf.high), 
            max.lo=first(conf.low),
            min = last(fit), 
            min.up=last(conf.high), 
            min.lo=last(conf.low)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(manu)


### WANING FOR DEATH
deaths_exp <- unvax %>% 
  filter(!ageRange%in%c("12-17","18-24")) %>%
  compute_expected(outcome = "deaths") %>%
  mutate(rate = fit / poblacion) %>%
  select(date, ageRange, gender, rate)

waning_deaths_dat <- waning %>% 
  filter(!ageRange%in%c("12-17","18-24")) %>%
  left_join(deaths_exp, by = c("date", "ageRange", "gender")) %>%
  mutate(exp = rate * poblacion) %>% 
  group_by(delta, ageRange, manu, day) %>%
  summarize(obs = sum(deaths), exp = sum(exp), .groups = "drop")

waning_deaths_fit <- waning_deaths_dat %>% 
  filter(!ageRange%in%c("12-17","18-24")) %>%
  filter(ageRange!="12-17") %>%
  group_by(manu, day) %>%
  summarize(obs = sum(obs), exp = sum(exp), .groups = "drop") %>%
  left_join(the_max_day, by="manu") %>%
  group_by(manu) %>%
  do(fit_waning_model(.))

waning_deaths_tab <- waning_deaths_fit %>% 
  group_by(manu) %>%
  arrange(day) %>%
  filter(day < one_max_day) %>%
  summarize(days = unique(max.day),
            day = first(day), 
            max = first(fit),
            max.up=first(conf.high), 
            max.lo=first(conf.low),
            min = last(fit), 
            min.up=last(conf.high), 
            min.lo=last(conf.low)) %>%
  mutate(manu = factor(manu, levels = manu_levels)) %>%
  arrange(manu)

####################################
## Hosp by Cases
###################################
hosp_by_cases <- counts_cases %>% 
  filter(!(ageRange == "12-17" & !manu %in% c("UNV", "PFR"))) %>%#&
    group_by(ageRange)%>%
  do(fit_prob_model(., outcome = "hosp"))

deaths_by_cases <- counts_cases %>% 
  #filter(manu != "JSN") %>%
  filter(!(ageRange == "12-17" & !manu %in% c("UNV", "PFR"))) %>% # &
           #!(ageRange %in% c("75-84", "85+") & manu == "JSN")) %>%
  group_by(ageRange)%>%
  do(fit_prob_model(., outcome = "deaths")) %>%
  mutate(zero = rate < 0.0001) %>%
  mutate(rate = ifelse(zero, 0, rate), 
         conf.low = ifelse(zero, 0, conf.low),
         conf.high = ifelse(zero, 0, conf.high)) 
  
ind <- which(deaths_by_cases$rate<10^-6) 
deaths_by_cases[ind, c("rate", "conf.low", "conf.high")] <- 0


## waning values
wa_eff <- sapply(c("MOD", "PFR", "JSN"), function(m){
  waning_tab %>% filter(manu==m) %>%
    dplyr::select(max, max.lo, max.up, min, min.lo, min.up) %>%
    mutate(max = make_pct(max), max.lo = round(max.lo*100), max.up = round(max.up*100),
           min = make_pct(min), min.lo = round(min.lo*100), min.up = round(min.up*100))
}, simplify = FALSE, USE.NAMES=TRUE)

wa_eff_hosp <- sapply(c("MOD", "PFR", "JSN"), function(m){
  waning_hosp_tab %>% filter(manu==m) %>%
    dplyr::select(day, max, max.lo, max.up, min, min.lo, min.up)%>%
    mutate(max = make_pct(max), max.lo = round(max.lo*100), max.up = round(max.up*100),
           min = make_pct(min), min.lo = round(min.lo*100), min.up = round(min.up*100))
}, simplify = FALSE, USE.NAMES=TRUE)

wa_eff_deaths <- sapply(c("MOD", "PFR", "JSN"), function(m){
  waning_deaths_tab %>% filter(manu==m) %>%
    dplyr::select(day, max, max.lo, max.up, min, min.lo, min.up)%>%
    mutate(max = make_pct(max), max.lo = round(max.lo*100), max.up = round(max.up*100),
           min = make_pct(min), min.lo = round(min.lo*100), min.up = round(min.up*100))
}, simplify = FALSE, USE.NAMES=TRUE)

tmp <- sapply(c("MOD", "PFR", "JSN"), function(m){
  waning_tab %>% filter(manu==m) %>%
    dplyr::select(max, max.lo, max.up, min, min.lo, min.up) 
}, simplify = FALSE, USE.NAMES=TRUE)

round_wa_est_mod <- paste0(round(100*tmp$MOD$min,-1), "%")
round_wa_est_pfr <- paste0(round(100*tmp$PFR$min,-1), "%")
round_wa_est_jsn <- paste0(round(100*tmp$JSN$min,-1), "%")

### expected and observed

exp_obs <- counts_cases %>% 
  left_join(cases_exp, by = c("date", "ageRange","gender")) %>%
  mutate(exp_cases = rate*poblacion) %>%
  select(-rate) %>%
  left_join(hosp_exp, by = c("date", "ageRange","gender")) %>%
  mutate(exp_hosp = rate*poblacion) %>% 
  select(-rate) %>%
  left_join(deaths_exp, by = c("date", "ageRange","gender")) %>%
  mutate(rate = replace_na(rate, 0)) %>%
  mutate(exp_deaths = rate*poblacion) %>% 
  select(-rate) %>%
  select(-poblacion) %>%
  group_by(ageRange, manu) %>%
  summarize(across(where(is.numeric), sum), .groups = "drop") 
  
### totals for tables

tmp <- counts_vax_age_gender_manu %>% filter(date == analysis_last_day) %>% group_by(manu) %>% summarize(n=sum(vax)) 
vax_tots <- pull(tmp, n)
names(vax_tots) <- pull(tmp, manu)

first_vax_day <- counts_vax_age_gender_manu %>% filter(date<=analysis_last_day) %>%
  filter(vax>0) %>% pull(date) %>% min
tmp <- counts_cases %>% filter(date >= first_vax_day)
ncases <- sum(tmp$cases)
nhosp <- sum(tmp$hosp)
ndeaths <- sum(tmp$deaths)

vax_cases_tots <- tmp %>%
  group_by(manu) %>%
  summarize(cases = sum(cases),
            hosp= sum(hosp),
            deaths= sum(deaths),
            poblacion = sum(poblacion)/365, .groups = "drop") 
