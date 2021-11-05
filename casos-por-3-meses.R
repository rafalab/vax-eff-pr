library(data.table)
library(lubridate)
library(tidyverse)
cases_url <- "https://bioportal.salud.gov.pr/api/administration/reports/cases/summary"

source("bp_credentials.R")

age_starts <- c(0, 12, 18, 25, seq(35, 85, 10))
age_ends <- c(age_starts[-1]-1, Inf)
age_levels <- paste(age_starts, age_ends, sep = "-")
age_levels[length(age_levels)] <- paste0(age_starts[length(age_levels)],"+")

### DOWNLOAD AND WRANGLE CASES

res <- httr::POST("https://bioportal.salud.gov.pr/api/authentication/login/",
           body= personal_data, 
           encode='json', 
           httr::content_type('application/json'))

token <- jsonlite::fromJSON(rawToChar(res$content))

dat <- httr::GET('https://bioportal.salud.gov.pr/api/administration/reports/cases/?caseType=Probable',
                 httr::content_type('application/json'), 
                 httr::add_headers(.headers=c('Authorization' = paste('Bearer', token$token), 
                                              'Accept-Enconding'="br")))

prob_dat <- jsonlite::fromJSON(rawToChar(dat$content), flatten = T)

# get_bioportal <- function(url){
#   jsonlite::fromJSON(
#     rawToChar(
#       httr::GET(url, httr::content_type('application/json'),
#                 httr::add_headers('Accept-Enconding'="br"))$content)
#   )
# }

parsed_dat <- rbind(conf_dat, prob_dat)

the_years <- seq(2020, year(today()))
imputation_delay  <- 2

all_tests_with_id <- parsed_dat #get_bioportal(cases_url)
tmp <- copy(all_tests_with_id)

setDT(all_tests_with_id)

all_tests_with_id[,earliestRankingTest.testType := str_to_title(earliestRankingTest.testType)]
#all_tests_with_id[,earliestRankingTest.testType == "Antigeno", earliestRankingTest.testType := "Antigens"]
all_tests_with_id[,earliestRankingTest.sampleCollectedDate := ymd_hms(earliestRankingTest.sampleCollectedDate, tz = "America/Puerto_Rico")]
all_tests_with_id[,earliestRankingTest.orderResult.reportedDate := ymd_hms(earliestRankingTest.orderResult.reportedDate, tz = "America/Puerto_Rico")]
all_tests_with_id[,earliestRankingTest.createdAt := ymd_hms(earliestRankingTest.createdAt, tz = "America/Puerto_Rico")]
all_tests_with_id[,earliestRankingTest.orderResult.createdAt := ymd_hms(earliestRankingTest.orderResult.createdAt, tz = "America/Puerto_Rico")]

# all_tests_with_id[ageRange=="N/A", ageRange := "NA"]
# all_tests_with_id[,result := tolower(result)]
# all_tests_with_id[,result := case_when(grepl("positive", result) ~ "positive",
#                                        grepl("negative", result) ~ "negative",
#                                        result == "not detected" ~ "negative",
#                                        TRUE ~ "other")]

all_tests_with_id[, date := earliestRankingTest.sampleCollectedDate]
all_tests_with_id[earliestRankingTest.sampleCollectedDate > earliestRankingTest.orderResult.reportedDate, date := earliestRankingTest.orderResult.reportedDate] ## if collectedDate is in the future make it reportedDate
all_tests_with_id[is.na(earliestRankingTest.sampleCollectedDate), date := earliestRankingTest.orderResult.reportedDate - days(imputation_delay)]
all_tests_with_id[, date := as_date(date)]
all_tests_with_id[!year(date) %in% the_years,  date := earliestRankingTest.orderResult.reportedDate - days(imputation_delay)]  
all_tests_with_id <-all_tests_with_id[year(date) %in% the_years & date <= today()]
all_tests_with_id[order(date, earliestRankingTest.orderResult.reportedDate)]    



##para el ejemplo añadir sexo
# all_tests_with_id$gender <- sample(c("M","F"), nrow(all_tests_with_id), replace = TRUE)

all_tests_with_id <- all_tests_with_id %>% 
  dplyr::select(caseId, caseType, patient.birthDate, patient.sex, patient.patientId,
                date) %>%
  mutate(patient.birthDate = ymd_hms(patient.birthDate)) %>%
  mutate(age = as.numeric(difftime(date, patient.birthDate, units="days"))/365.25) %>% 
  mutate(ageRange = cut(age, c(age_starts, Inf), right = FALSE, labels = age_levels)) %>%
  mutate(ageRange = factor(ageRange, levels = age_levels)) %>%
  rename(gender = patient.sex, patientId =  patient.patientId) %>% 
  mutate(gender = recode(gender, Female = "F",   Male = "M", 
                         Other = "O",  Unknown = "U")) %>%
  mutate(gender = factor(gender, levels = unique(gender)))

### COUNT CASES in 90-day WINDOWS
span <- 90 
first_day <- lubridate::make_date(2020, 12, 15)
last_day <- max(all_tests_with_id$date)
dates <- seq(first_day - days(span), last_day, by = "day")

dat <- all_tests_with_id#[testType %in% c("Molecular", "Antigens") & result == "positive",]
cases <- lapply(dates, function(d){
  tmp <- dat[date <= d & date > d - days(span), 
      .(cases = uniqueN(patientId)), by =c("gender", "ageRange")]
  tmp$date <- d
  return(tmp)
})
cases <- do.call(rbind, cases)  

save(cases, file = "rdas/cases_3_months.rda")
cases %>% group_by(date) %>% summarize(cases=sum(cases)) %>% ggplot(aes(date, cases)) + geom_line() #+ facet_wrap(~ageRange)
cases %>% ggplot(aes(date, cases, color = gender)) + geom_line() + facet_wrap(~ageRange)

