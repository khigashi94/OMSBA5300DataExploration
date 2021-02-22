#Libraries

library(car)
library(vtable)
library(jtools)
library(vtable)
library(purrr)
library(tidyverse)
library(dplyr)


## Load/Join Data Tables

# Bind "trends_up_to_" data
trends_all <- data.frame()
for (data in list.files(pattern = "trends_up_to_")){
  temp <- read.csv(data)
  trends_all <- rbind(trends_all, temp)
}

# Scorecard and ID CSVs
score_card <- read.csv("Most+Recent+Cohorts+(Scorecard+Elements).csv")
id_name <- read.csv("id_name_link.csv")%>%
  distinct(schname, .keep_all = TRUE)
coll_ids <- left_join(id_name, score_card, by = c("unitid"="UNITID"))


# Full join - Removes all "not currently open institutions"
df_undg <- left_join(coll_ids, trends_all, by = c("schname" = "schname"))%>%
  filter(PREDDEG == 3, !is.null(monthorweek), CURROPER == 1)%>% 
  mutate(num_earnings = as.numeric(md_earn_wne_p10.REPORTED.EARNINGS), yr = as.Date(str_sub(monthorweek, 1, 4), "%Y")) %>%
  mutate(key = paste(opeid,monthorweek, sep = "_"), date_state = paste(STABBR, monthorweek, sep="_"))%>%
  # select(-unitid, -opeid, -OPEID, -opeid6, -monthorweek)%>%
  filter(!is.na(num_earnings))%>%
  
  # Mutate variables to correct class(es)
  select(key, date_state, unitid, opeid, opeid6,	schname, INSTNM, STABBR, yr, monthorweek, STABBR, num_earnings, keyword, keynum, index, c(HBCU:WOMENONLY), RELAFFIL, DISTANCEONLY, starts_with("PCIP"), starts_with("UGDS"), ACTCMMID, SAT_AVG, SAT_AVG_ALL, starts_with("NPT"), starts_with("RET"), starts_with("GRAD"), RPY_3YR_RT_SUPP, PCTFLOAN, PCTPELL, UG25abv, C150_4_POOLED_SUPP.REPORTED.GRAD.RATE, PPTUG_EF)%>%
  mutate_at(vars(c(HBCU:WOMENONLY), RELAFFIL, DISTANCEONLY), as.factor)%>%
  mutate_at(vars(matches("PCIP"), matches("UGDS")), as.numeric)%>%
  mutate_at(vars(matches("NPT"), matches("RET"), matches("GRAD"), RPY_3YR_RT_SUPP, PCTFLOAN, PCTPELL), as.numeric)%>%
  mutate(UG25abv = as.numeric(UG25abv), C150_4_POOLED_SUPP.REPORTED.GRAD.RATE= as.numeric( C150_4_POOLED_SUPP.REPORTED.GRAD.RATE), PPTUG_EF = as.numeric(PPTUG_EF))%>%
  mutate_at(vars(ACTCMMID, SAT_AVG, SAT_AVG_ALL), as.numeric)%>%
  group_by(schname, monthorweek)%>%
  mutate(ct_index = sum(index), num_keywords = max(keynum))%>%
  ungroup()%>%
  filter(!is.na(monthorweek), !is.na(num_earnings))%>%
  
  group_by(date_state)%>%
  mutate(limit_earnings = mean(num_earnings) + sd(num_earnings))%>%
  ungroup()%>%

  mutate(exc_earning = num_earnings>= limit_earnings)%>%
  filter(!is.na(exc_earning))
# 
# # Determine average yearly earnings, adjust with stdev
# avg_earnings <- df_undg %>%
#   # select(date_state, monthorweek, STABBR, num_earnings)%>%
#  
#   transmute(date_state,limit_earnings = mean+sd)%>%
#   ungroup()
# 
# 
# 
# checke <- left_join(df_undg, select(avg_earnings, c(date_state, limit_earnings)), by = "date_state")
#   mutate(over_lim = num_earnings >= limit_earnings)
# 
# 
# earngs <- df_undg%>%
#   left_join(avg_earnings, by="date_state")
# 
# 
# 
# 
# exc_earn <- df_undg%>%
#   # select(key, date_state, num_earnings)%>%
#   left_join(avg_earnings, by = "date_state")%>%
#   mutate(over_lim = num_earnings >= limit_earnings)
# # %>%
#   # select(key, over_lim)%>%
#   # distinct()
# 
#   
# 
# 
# # Merge keyw_ct and avg_earnings
# key_earnings <- avg_earnings%>%
#   left_join(keyw_ct, by= c("monthorweek", "STABBR"), copy = FALSE )%>%
#   drop_na()%>%
#   mutate(high_earn = limit_earnings<num_earnings)%>%
#   select(unitid, schname, monthorweek, limit_earnings, keynum, index, ct_index, num_keywords, high_earn)
# 
# 
# # 
# # 
# # ## Regress key earnings
# # reg_keyword_earnings <- lm(ct_index ~ high_earn + num_keywords, data = key_earnings)
# # export_summs(reg_keyword_earnings)
# 
# 
# # 
# # test_ <- lm(ct_index)
# # regression <- lm(ct_index~ excess + , data = merged)
# # export_summs(regression)
# # 
# 
# 
# 
# 
