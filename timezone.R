library(tidyverse)
library(haven)
library(stringr)
library(lubridate)
library(clock)
library(caret)

meof_usa_survey_df = readRDS('survey/meof_usa_survey_df.RDS')
meof_usa_web_df = readRDS('tracking/meof_usa_web_df.RDS')
time_zones = readRDS('tracking/time_zones.RDS')

meof_usa_survey_wave3 = filter(meof_usa_survey_df, wave == 3) %>% select(personid,inputstate) %>% unique() #Here we , we use the third wave of the survey as it took place in roughly the same time period as that of the tracking data we have.

labels = rownames_to_column(as.data.frame(attributes(meof_usa_survey_wave3[[2]])$labels)) %>%
  set_names(c('state','inputstate'))

timeadjustmentdf_wave3 = left_join(meof_usa_survey_wave3,labels) %>% left_join(time_zones) %>%
  select(personid,time_zone)#Once they have all been joined we only need the personid and timezone for adjustment

timeadjustmentdf_wave1 = left_join(filter(meof_usa_survey_df, wave == 1) %>% select(personid,inputstate),labels) %>% left_join(time_zones) %>%
  select(personid,time_zone)#Once they have all been joined we only need the personid and timezone for adjustment

timeadjustmentdf_wave2 = left_join(filter(meof_usa_survey_df, wave == 2) %>% select(personid,inputstate),labels) %>% left_join(time_zones) %>%#Taking 
  select(personid,time_zone)#Once they have all been joined we only need the personid and timezone for adjustment

#ASIDE:
#bothtimeadjusteddfs = full_join(timeadjustmentdf,timeadjustmentdf2, by = 'personid')
#confusionMatrix(data = as.factor(bothtimeadjusteddfs$time_zone.y), reference = as.factor(bothtimeadjusteddfs$time_zone.x))
#This code shows us that the state values from the first wave have a very high level of agreement (over 99% sensitivity and specificity) with the inputstates from wave 3, so it is a fair assumption to use the wave 1 timezones to impute in cases where we do not have a wave 3 timezone.

timeadjustmentdf3 = full_join(timeadjustmentdf_wave3, timeadjustmentdf_wave2, by = 'personid') %>%
  mutate(time_zone = coalesce(time_zone.x, time_zone.y)) %>%
  select(personid, time_zone)

timeadjustmentdf4 = full_join(timeadjustmentdf3, timeadjustmentdf_wave1, by = 'personid') %>%
  mutate(time_zone = coalesce(time_zone.x, time_zone.y)) %>%
  select(personid, time_zone)

meof_usa_web_df2 = left_join(meof_usa_web_df,timeadjustmentdf4, by = 'personid')

#attributes(meof_usa_web_df[[2]])

#sum(is.na(meof_usa_web_df2$time_zone))/length(meof_usa_web_df2$time_zone)

meof_usa_web_df3 <- meof_usa_web_df2 %>%
  na.omit() %>%
  rowwise() %>%
  mutate(used_at_local = as.character(with_tz(used_at, time_zone))) %>% #Changes the timezones to be in the local time of the participant, stores the final vector as text becasue otherwise lubridate sets all the times in a column to be the timezone of the first element.
  ungroup()

############

subset = meof_usa_web_df2[seq(1, nrow(meof_usa_web_df2), by = 10000),] %>% na.omit()
subset2 <- subset %>%
  rowwise() %>%
  mutate(used_at_local = as.character(with_tz(used_at, time_zone))) %>% #Changes the timezones to be in the local time of the participant, stores the final vector as text becasue otherwise lubridate sets all the times in a column to be the timezone of the first element.
  ungroup()

############# IGNORE

meof_usa_web_df3$adjusted_times = pmap(list(meof_usa_web_df2$used_at, meof_usa_web_df2$time_zone), with_tz)
meof_usa_web_df3$adjusted_times = as.POSIXlt(meof_usa_web_df3$adjusted_times)


subset
subset2 = subset %>% mutate(used_at_local = force_tzs(used_at, tzones = subset$time_zone))

with_tz(subset$used_at[4],tzone = 'US/Pacific')

subset2 <- subset %>% rowwise() %>% print()
  #mutate(adjusted_times = as.POSIXlt(with_tz(used_at, time_zone)))

subset2$adjusted_times = map2_vec(subset2$used_at,subset2$time_zone, with_tz)
subset2$adjusted_times = as.POSIXlt.numeric(subset2$adjusted_times)

as.factor()

subset2 <- subset %>%
  rowwise() %>%
  mutate(adjusted_times = as.character(with_tz(used_at, time_zone))) %>%
  ungroup()

subset2$adjusted_times[15]

as.POSIXlt()

subset %>%
  group_by(time_zone)%>%
  mutate(result_column = map2(used_at, .group, with_tz))

for timezone in subset2$

#%>% rowwise()

as_datetime()

dst('2018-10-09 16:44:39')

subset$used_at <- as.POSIXct(subset$used_at, tz = "UTC")

subset2 <- subset %>% rowwise() %>%
  do(timestamp_local = with_tz(.$used_at, tzone = .$time_zone))

as.POSIXlt(as.numeric(subset2[7,]),tz='US/Pacific')


get_local_time <- function(timestamp_utc, local_tz) {
  l <- lapply(seq(length(timestamp_utc)), 
              function(x) {with_tz(timestamp_utc[x], local_tz[x])})
  combine(l)
}

subset2 = mutate(subset, timestamp_local = get_local_time(used_at, time_zone))
subset2 = c(1,2,4,5)


data$timestamp_utc <- as.POSIXct(data$timestamp_utc, tz = "UTC")

subset2 = subset %>% 
  group_by(time_zone) %>%
  mutate(timestamp_local = with_tz(used_at, time_zone))

