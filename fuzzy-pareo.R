library(tidyverse)
library(stringdist)
library(fuzzyjoin)
library(lubridate)
library(dplyr)

source("init.R")
source("funcs.R")

# 1 - nombre, segundo_nombre, apellido, segundo apellido
# 2 - nombre, inicial, apellido, segundo apellido
# 3 - nombre,  apellido, segundo apellido
# 4 - nombre, segundo_nombre, apellido,
# 5 - nombre, inicial, apellido
# 6 - nombre apellido
# 7 - como #3 pero con apellidos invertidos

fix_names <- function(x){
  replace_na(x, "") %>%
    str_remove_all("\\s+") %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ü", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_remove_all("[^a-zA-Z\\s]+") %>%
    str_trim()
}
#
## covert del valle to delvalle, de la 
join_dela <- function(x){
  str_replace_all(x, "^(y|de|la|las|los|del|lo|di|da|le|st)\\s+", "\\1") %>%
    str_replace_all("^(dela|delas|delos|delo)\\s+", "\\1") %>%
    str_replace_all("(mc|mac|van|san)\\s+", "\\1")
}

get_names_preis <- function(tab){
  tab %>% 
    mutate(PAT_FIRST_NAME = replace_na(PAT_FIRST_NAME, ""),
           PAT_MIDDLE_NAME = replace_na(PAT_MIDDLE_NAME, ""), 
           PAT_LAST_NAME = replace_na(PAT_LAST_NAME, "")) %>%
    mutate(original = paste(PAT_FIRST_NAME, PAT_MIDDLE_NAME, PAT_LAST_NAME, sep="|")) %>% 
    mutate_at(vars(PAT_FIRST_NAME,  PAT_MIDDLE_NAME, PAT_LAST_NAME), tolower) %>%
    mutate_at(vars(PAT_FIRST_NAME,  PAT_MIDDLE_NAME, PAT_LAST_NAME), join_dela) %>% 
    separate(PAT_LAST_NAME, c("PAT_LAST_NAME", "last_name_2"), sep = "\\s+", remove = TRUE, extra = "merge", fill = "right") %>%
    separate(PAT_FIRST_NAME, c("PAT_FIRST_NAME", "PAT_MIDDLE_NAME_2"), sep = "\\s+", remove = TRUE, extra = "merge", fill = "right") %>%
    mutate(last_name_2 = replace_na(last_name_2, ""),
           PAT_MIDDLE_NAME_2 = replace_na(PAT_MIDDLE_NAME_2, "")) %>% 
    mutate_at(vars(last_name_2, contains("PAT_")), join_dela) %>%
    mutate(PAT_MIDDLE_NAME =  ifelse(PAT_MIDDLE_NAME == "", PAT_MIDDLE_NAME_2, PAT_MIDDLE_NAME)) %>%
    mutate(first_name = PAT_FIRST_NAME,
           middle_name = PAT_MIDDLE_NAME,
           middle_initial = str_sub(PAT_MIDDLE_NAME, 1, 1),
           last_name_1 = PAT_LAST_NAME) %>% 
    mutate_at(vars(first_name, middle_name, middle_initial, last_name_1, last_name_2), fix_names) %>%
    mutate(flag_middle_name = nchar(middle_name) > 1,
           flag_middle_initial = middle_initial != "",
           flag_last_name_2 = last_name_2 != "") %>%
    mutate(pattern_1 = ifelse(flag_middle_name  & flag_last_name_2, paste(first_name, middle_name, last_name_1, last_name_2), NA),
           pattern_2 = ifelse(flag_middle_initial & flag_last_name_2, paste(first_name, middle_initial, last_name_1, last_name_2), NA),
           pattern_3 = ifelse(flag_last_name_2, paste(first_name, last_name_1, last_name_2), NA),
           pattern_4 = ifelse(flag_middle_name, paste(first_name, middle_name, last_name_1), NA),
           pattern_5 = ifelse(flag_middle_initial, paste(first_name, middle_initial, last_name_1), NA),
           pattern_6 = paste(first_name, last_name_1),
           pattern_7 = pattern_3) %>%
    mutate(n_pattern_1 = nchar(pattern_1),
           n_pattern_2 = nchar(pattern_2),
           n_pattern_3 = nchar(pattern_3),
           n_pattern_4 = nchar(pattern_4),
           n_pattern_5 = nchar(pattern_5),
           n_pattern_6 = nchar(pattern_6),
           n_pattern_7 = n_pattern_3) %>%
    select(PATIENT_ID, original, DOB, first_name, middle_name, middle_initial, last_name_1, last_name_2, contains("pattern"))
}

get_names_bp <- function(tab){
  tab %>% 
    mutate(patient.firstName = replace_na(patient.firstName, ""),
           patient.middleName = replace_na(patient.middleName, ""), 
           patient.lastName = replace_na(patient.lastName, ""),
           patient.secondLastName = replace_na(patient.secondLastName, "")) %>%
    mutate(original = paste(patient.firstName, patient.middleName, patient.lastName, patient.secondLastName, sep="|")) %>%
    mutate_at(vars(patient.firstName, patient.middleName, patient.lastName, patient.secondLastName), tolower) %>%
    mutate(tmp = patient.lastName %in% c("de", "del", "de la", "de las", "de los","de lo", "da ", "do ", "di ", "mac ", "mc ", "le ", "san ", "van ", "st ")) %>%
    mutate(patient.lastName = ifelse(tmp, paste0(patient.lastName, patient.secondLastName), patient.lastName)) %>%
    mutate(patient.secondLastName = ifelse(tmp, "", patient.secondLastName)) %>%
    select(-tmp) %>%
    mutate_at(vars(patient.firstName, patient.middleName, patient.lastName, patient.secondLastName), join_dela) %>%
    separate(patient.lastName, c("patient.lastName", "last_name_2"), sep = "\\s+", remove = TRUE, extra = "merge", fill = "right") %>%
    separate(patient.firstName, c("patient.firstName", "patient.middleName.2"), sep = "\\s+", remove = TRUE, extra = "merge", fill = "right") %>%
    mutate(last_name_2 = replace_na(last_name_2, ""),
           patient.middleName.2 = replace_na(patient.middleName.2, "")) %>%
    mutate_at(vars(patient.firstName, patient.middleName.2, patient.middleName, patient.lastName, patient.secondLastName, last_name_2), join_dela) %>%
    mutate(patient.middleName =  ifelse(patient.middleName == "", patient.middleName.2, patient.middleName)) %>%
    mutate(last_name_2 =  ifelse(patient.secondLastName == "", last_name_2, patient.secondLastName)) %>%
    mutate(first_name = patient.firstName,
           middle_name = patient.middleName,
           middle_initial = str_sub(patient.middleName, 1, 1),
           last_name_1 = patient.lastName) %>%
    mutate_at(vars(first_name, middle_name, middle_initial, last_name_1, last_name_2), fix_names) %>%
    mutate(flag_middle_name = nchar(middle_name) > 1,
           flag_middle_initial = middle_initial != "",
           flag_last_name_2 = last_name_2 != "") %>%
    mutate(pattern_1 = ifelse(flag_middle_name & flag_last_name_2, paste(first_name, middle_name, last_name_1, last_name_2), NA),
           pattern_2 = ifelse(flag_middle_initial & flag_last_name_2, paste(first_name, middle_initial, last_name_1, last_name_2), NA),
           pattern_3 = ifelse(flag_last_name_2, paste(first_name, last_name_1, last_name_2), NA),
           pattern_4 = ifelse(flag_middle_name, paste(first_name, middle_name, last_name_1), NA),
           pattern_5 = ifelse(flag_middle_initial, paste(first_name, middle_initial, last_name_1), NA),
           pattern_6 = paste(first_name, last_name_1),
           pattern_7 = ifelse(flag_last_name_2 & last_name_1 != last_name_2, paste(first_name, last_name_2, last_name_1), NA)) %>%
    mutate(n_pattern_1 = nchar(pattern_1),
           n_pattern_2 = nchar(pattern_2),
           n_pattern_3 = nchar(pattern_3),
           n_pattern_4 = nchar(pattern_4),
           n_pattern_5 = nchar(pattern_5),
           n_pattern_6 = nchar(pattern_6),
           n_pattern_7 = nchar(pattern_7)) %>%
        rename(DOB = "patient.birthDate") %>%
    select(caseId, original, DOB, first_name, middle_name, middle_initial, last_name_1, last_name_2, contains("pattern")) 
}

### Read PREIS -----------------------------------------------------------------

# Read in vaccine records ------------------------------------------------------
# this file is not publically shared so this wont run

load("rdas/vax_data_BP.rda")

preis<- vax_data_BP %>% #, locale = readr::locale(encoding = "ISO-8859-1")) %>%
  mutate(PATIENT_CITY = str_to_title(PATIENT_CITY)) %>%
  mutate(PATIENT_CITY = fix_municipio(PATIENT_CITY)) %>%
  mutate(PATIENT_CITY = na_if(PATIENT_CITY, "")) %>%
  mutate(manu = case_when(VACC_CODE == "2080" ~ "MOD",
                          VACC_CODE == "2089" ~ "PFR",
                          VACC_CODE == "2091" ~ "ATZ", 
                          VACC_CODE == "2092" ~ "JSN", 
                          VACC_CODE == "2090" ~ "OTR")) %>%
  mutate(ADMINISTRATION_DATE = as_date(ADMINISTRATION_DATE), 
         DOB = as_date(DOB)) %>%
  arrange(ADMINISTRATION_DATE) %>%
  group_by(PATIENT_ID) %>%
  mutate(dose = seq_along(ADMINISTRATION_DATE)) %>%
  ungroup() 

preis <- full_join(filter(preis, dose == 1), 
                     select(filter(preis, dose == 2), PATIENT_ID, FACILITY_NAME, ADMINISTRATION_DATE, dose, manu),
                     by = "PATIENT_ID", suffix = c("_1", "_2")) 

# Read BioPortal ---------------------------------------------------------------

bp <- readRDS("rdas/names.RDS") %>% 
  mutate(patient.physicalCity = na_if(na_if(patient.physicalCity,"No Domiciliado"),"N/A")) %>%
  mutate(patient.birthDate = as_date(patient.birthDate)) %>%
  mutate(patient.physicalCity = recode(patient.physicalCity, Loiza = "Loíza", `Rio Grande`="Río Grande")) %>%
  filter(patient.physicalCity %in% unique(preis$PATIENT_CITY)) %>%
  mutate(patient.sex = recode(patient.sex, Female = "F", Male = "M", Other = "O", Unknown = "U"))

year(bp$patient.birthDate[which(year(bp$patient.birthDate) == 57)])  <- 1957
year(bp$patient.birthDate[which(year(bp$patient.birthDate) == 1779)])  <- 1979
year(bp$patient.birthDate[which(year(bp$patient.birthDate) > 2021)])  <- year(bp$patient.birthDate[which(year(bp$patient.birthDate) > 2021)]) -100

#### TEST MATCH 
preis_names <- get_names_preis(preis) %>%
  filter(last_name_1!="")

bp_names <- get_names_bp(bp) 

## Find perfect matches by each pattern FUNCS ----------------------------------
perfect_match <- map_df(1:7, function(i){
  the_pattern <- paste0("pattern_", i)
  
  query <- bp_names %>% mutate(pattern = !!sym(the_pattern)) %>%
    filter(!is.na(pattern)) %>%
    select(caseId, pattern, DOB)
  target <- preis_names %>% rename(pattern = !!sym(the_pattern)) %>%
    filter(!is.na(pattern)) %>%
    select(PATIENT_ID, pattern, DOB)
  
  left_join(query, target, by = c("pattern", "DOB")) %>% 
    mutate(pattern_bp = pattern, pattern_preis = pattern) %>%
    select(-pattern, -DOB) %>%
    mutate(type = i, dist = 0, score = 1) %>%
    filter(!is.na(PATIENT_ID))
})


max_dists <- c(6, 5, 4, 4, 3, 3, 4)

fuzzy_match <- map_df(1:7, function(i){
 
  cat("\nPattern",i,"\n")
  pattern <- paste0("pattern_", i)
  n_pattern <- paste0("n_pattern_", i)
  
  query <- select(bp_names, all_of(c("caseId", "DOB", pattern, n_pattern))) %>% 
    setNames(c("caseId", "DOB", "pattern", "n_pattern")) %>% 
    filter(!is.na(pattern) & !caseId %in% pull(filter(perfect_match, type == i), "caseId"))
  
  target <- select(preis_names, all_of(c("PATIENT_ID", "DOB", pattern, n_pattern))) %>% 
    setNames(c("PATIENT_ID", "DOB", "pattern", "n_pattern")) %>% 
    filter(!is.na(pattern))

  DOBs_1 <- split(1:nrow(query), query$DOB)
  DOBs_2 <- split(1:nrow(target), target$DOB)

  common <- intersect(names(DOBs_1), names(DOBs_2))
  DOBs_1 <- DOBs_1[common]
  DOBs_2 <- DOBs_2[common]
  pb <-  txtProgressBar( 1, length(common), style = 3)
  res <- lapply(seq_along(common), function(j){
    setTxtProgressBar(pb, j)
   
    stringdist_left_join(query[DOBs_1[[j]],],
                         target[DOBs_2[[j]],],
                         by = "pattern",
                         method = "lv",
                         distance_col = "dist", max_dist = max_dists[i]) %>%
      group_by(caseId) %>% slice_min(dist) %>% ungroup() %>%
      mutate(score = 1 - dist/n_pattern.x, type = i) %>%
      mutate(pattern_bp = pattern.x, pattern_preis = pattern.y) %>%
      select(all_of(colnames(perfect_match)))
    
  })
  do.call(bind_rows, res)
})

fuzzy_match_reverse_date <- map_df(1:7, function(i){
  
  cat("\nPattern",i,"\n")
  pattern <- paste0("pattern_", i)
  n_pattern <- paste0("n_pattern_", i)
  
  out <- unique(c(pull(dplyr::filter(perfect_match, type == i), "caseId"), 
                  pull(dplyr::filter(fuzzy_match, type == i), "caseId")))
  
  query <-  select(bp_names, all_of(c("caseId", "DOB", pattern, n_pattern))) %>% 
    setNames(c("caseId", "DOB", "pattern", "n_pattern")) %>% 
    filter(!is.na(pattern) & !caseId %in% out) %>%
    filter(day(DOB)<=12 & day(DOB)!=month(DOB)) %>%
    mutate(DOB = ydm(DOB))
  
  target <- select(preis_names, all_of(c("PATIENT_ID", "DOB", pattern, n_pattern))) %>% 
    setNames(c("PATIENT_ID", "DOB", "pattern", "n_pattern")) %>% 
    filter(!is.na(pattern))
  
  DOBs_1 <- split(1:nrow(query), query$DOB)
  DOBs_2 <- split(1:nrow(target), target$DOB)
  
  common <- intersect(names(DOBs_1), names(DOBs_2))
  DOBs_1 <- DOBs_1[common]
  DOBs_2 <- DOBs_2[common]
  pb <-  txtProgressBar(1, length(common), style = 3)
  res <- lapply(seq_along(common), function(j){
    setTxtProgressBar(pb, j)
    
    stringdist_left_join(query[DOBs_1[[j]],],
                         target[DOBs_2[[j]],],
                         by = "pattern",
                         method = "lv",
                         distance_col = "dist", max_dist = max_dists[i]) %>%
      group_by(caseId) %>% slice_min(dist) %>% ungroup() %>%
      mutate(score = 1 - dist/n_pattern.x, type = i) %>%
      mutate(pattern_bp = pattern.x, pattern_preis = pattern.y) %>%
      select(all_of(colnames(perfect_match)))
    
  })
  do.call(bind_rows, res)
})


# match ------------------------------------------------------------------------

map <- bind_rows(perfect_match, fuzzy_match) %>% mutate(DOB_reversed = FALSE) %>%
  bind_rows(mutate(fuzzy_match_reverse_date, DOB_reversed = TRUE))

freq_first_name <- data.frame(first_name = c(preis_names$first_name, preis_names$middle_name)) %>%
  filter(first_name != "" & nchar(first_name) > 1) %>%
  group_by(first_name) %>%
  summarize(freq = n()) %>% 
  mutate(freq = freq / sum(freq))

freq_last_name <- data.frame(last_name = c(preis_names$last_name_1, preis_names$last_name_2)) %>%
  filter(last_name != "" & nchar(last_name) > 1) %>%
  group_by(last_name) %>%
  summarize(freq = n()) %>% 
  mutate(freq = freq / sum(freq)) 

tmp1 <- map %>% 
  left_join(select(bp_names, caseId, original, first_name, middle_name, last_name_1, last_name_2), by = "caseId") %>%
  left_join(freq_first_name, by = "first_name") %>% 
  mutate(freq = replace_na(freq, min(freq_first_name$freq))) %>%
  rename(freq_first_name = freq) %>%
  left_join(freq_first_name, by = c("middle_name" = "first_name")) %>% 
  mutate(freq = replace_na(freq, min(freq_last_name$freq))) %>%
  rename(freq_middle_name = freq) %>%
  left_join(freq_last_name, by = c("last_name_1" = "last_name")) %>%
  mutate(freq = replace_na(freq, min(freq_last_name$freq))) %>%
  rename(freq_last_name_1 = freq) %>%
  left_join(freq_last_name, by = c("last_name_2" = "last_name")) %>%
  rename(freq_last_name_2 = freq) %>%
  left_join(select(bp, caseId, patient.physicalCity, patient.sex), by = "caseId") %>%
  rename(municipio = patient.physicalCity, gender = patient.sex) %>%
  setNames(c("caseId", "PATIENT_ID", "pattern_bp", "pattern_preis", "type", "dist", "score", "DOB_reversed",
            paste0(c("original", "first_name", "middle_name", "last_name_1", "last_name_2", "freq_first_name", "freq_middle_name", "freq_last_name_1",
                   "freq_last_name_2", "municipio", "gender"), "_bp")))  
tmp2 <- map %>% 
  left_join(select(preis_names, PATIENT_ID, original, first_name, middle_name, last_name_1, last_name_2), by = "PATIENT_ID") %>%
  left_join(freq_first_name, by = "first_name") %>% 
  rename(freq_first_name = freq) %>%
  left_join(freq_first_name, by = c("middle_name" = "first_name")) %>% 
  rename(freq_middle_name = freq) %>%
  left_join(freq_last_name, by = c("last_name_1" = "last_name")) %>%
  rename(freq_last_name_1 = freq) %>%
  left_join(freq_last_name, by = c("last_name_2" = "last_name")) %>%
  rename(freq_last_name_2 = freq) %>%
  left_join(select(preis, PATIENT_ID, PATIENT_CITY, PATIENT_GENDER), by = "PATIENT_ID") %>%
  rename(municipio = PATIENT_CITY, gender = PATIENT_GENDER) %>%
  select(original, first_name, middle_name, last_name_1, last_name_1, last_name_2, 
         freq_first_name, freq_middle_name, freq_last_name_1, freq_last_name_2, municipio, gender) %>%
  setNames(paste0(c("original", "first_name", "middle_name", "last_name_1", "last_name_2", 
                    "freq_first_name", "freq_middle_name", "freq_last_name_1",
                    "freq_last_name_2", "municipio", "gender"), "_preis")) 

bp_preis_matches <- bind_cols(tmp1, tmp2) %>%
  mutate(freq_first_name = pmax(freq_first_name_bp, freq_first_name_preis, na.rm = TRUE),
         freq_middle_name = pmax(freq_middle_name_bp, freq_middle_name_preis, na.rm = TRUE),
         freq_last_name_1 = pmax(freq_last_name_1_bp, freq_last_name_1_preis, na.rm = TRUE),
         freq_last_name_2 = pmax(freq_last_name_2_bp, freq_last_name_2_preis, na.rm = TRUE)) %>%
  mutate(first_name_score = 1 - stringdist(first_name_bp, first_name_preis, method = "lv") / max(nchar(first_name_bp), nchar(first_name_preis)), 
         middle_name_score = 1 - stringdist(middle_name_bp, middle_name_preis, method = "lv") / max(nchar(middle_name_bp), nchar(middle_name_preis)), 
         last_name_1_score = 1 - stringdist(last_name_1_bp, last_name_1_preis, method = "lv") / max(nchar(last_name_1_bp), nchar(last_name_1_preis)), 
         last_name_2_score = 1 - stringdist(last_name_2_bp, last_name_2_preis, method = "lv") / max(nchar(last_name_2_bp), nchar(last_name_2_preis))) %>%
  mutate(first_name_initial_match = str_sub(first_name_bp, 1, 1) == str_sub(first_name_preis, 1, 1),
         middle_name_initial_match = str_sub(middle_name_bp, 1, 1) == str_sub(middle_name_preis, 1, 1),
         last_name_1_initial_match = str_sub(last_name_1_bp, 1, 1) == str_sub(last_name_1_preis, 1, 1),
         last_name_2_initial_match = str_sub(last_name_2_bp, 1, 1) == str_sub(last_name_2_preis, 2, 1)) %>%
  select(caseId, PATIENT_ID,
         original_bp, original_preis, 
         pattern_bp, pattern_preis,
         municipio_bp, municipio_preis,
           gender_bp, gender_preis,
         type, dist, score, DOB_reversed,
         first_name_bp, first_name_preis, first_name_score, freq_first_name, first_name_initial_match,
         middle_name_bp, middle_name_preis, middle_name_score, freq_middle_name, middle_name_initial_match,
         last_name_1_bp, last_name_1_preis, last_name_1_score, freq_last_name_1, last_name_1_initial_match,
         last_name_2_bp, last_name_2_preis, last_name_2_score, freq_last_name_2, last_name_2_initial_match)


fit_glm <- function(tab){
  dat <- tab %>% mutate(y = municipio_bp==municipio_preis & 
                         gender_bp == gender_preis) %>%
    filter(!is.na(y) & !is.na(score) &
             !is.na(freq_first_name) & !is.na(freq_last_name_1) &
             !is.na(first_name_initial_match) & !is.na(last_name_1_initial_match))
  if(dat$DOB_reversed[1] & dat$type[1] == 1){
    fit <- glm(y ~ score + log(freq_first_name) + log(freq_last_name_1), family = "binomial", data = dat)
  } else{
    fit <- glm(y ~ score + log(freq_first_name) + log(freq_last_name_1) + 
                 first_name_initial_match + last_name_1_initial_match, family = "binomial", data = dat)
  }
  tab %>% mutate(match_score = predict(fit, newdata = tab, type = "response"))
}

### Checks based on city match
bp_preis_matches <- bp_preis_matches %>% 
  group_by(type, DOB_reversed) %>%
  do(fit_glm(.)) %>% 
  ungroup()

saveRDS(bp_preis_matches, file="rdas/bp_preis_matches.rds", compress="xz")
## bp_preis_matches <- readRDS("rdas/bp_preis_matches.rds")

bp_preis_map <- bp_preis_matches %>% 
  filter(match_score > 0.7 | (match_score > 0.4 & municipio_bp == municipio_preis & 
                                    gender_bp == gender_preis)) %>%
  mutate(tie_breaker = as.numeric(municipio_bp == municipio_preis)) %>%
  mutate(tie_breaker = replace_na(tie_breaker, 0)) %>%
  mutate(final_score = round(match_score*100) + (7-type)/10 + tie_breaker/100 + as.numeric(!DOB_reversed)/1000) %>% 
  group_by(caseId) %>%
  slice_max(final_score) %>%
  ungroup() %>%
  group_by(caseId, PATIENT_ID) %>% ##remove ties that match to same preis record.
  slice(1) %>%
  ungroup() %>%
  select(-final_score,-tie_breaker)

saveRDS(bp_preis_map, file="rdas/bp_preis_map.rds", compress="xz")




## PREIS DUPLICATES
if(FALSE){
tmp <- preis_names %>% filter(!is.na(pattern_3)) %>% 
  select(PATIENT_ID, DOB, pattern_3)%>%
  mutate(x = paste(pattern_3, DOB)) %>% 
  group_by(x) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n>1) %>%
  left_join(select(preis,c(1:11,13,24)), by = "PATIENT_ID") %>%
  arrange(x) %>%
  select(-c(2:5))
write.csv(tmp, file = "~/Desktop/preis-duplicates.csv")
}