#####
library(dplyr)
library(tidyr)
library(snakecase)

a <- result_before_matching
b <- result_after_matching12
c <- result_after_matching01

#create smd before matching
#unequal number of rows per period caused by small numbers in age group 100-150#
a <- a %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Prior observation", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
                period = ifelse(period >16, period - 16, period),
                group = gsub(".*? ", "", group_level)) 

#Continuous variable
a1 <- a %>%
  dplyr::filter(estimate_type %in% c("mean", "sd")) %>%
  dplyr::mutate(estimate = as.numeric(estimate)) %>%
  pivot_wider(
    names_from = estimate_type,
    values_from = estimate
  )%>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, sd)
  )%>%
  dplyr::mutate(smd1 = (mean_cohort1 - mean_cohort2)/(sqrt((1/2)*((sd_cohort1)^2+(sd_cohort2)^2))),
                smd2 = (mean_target - mean_cohort1)/(sqrt((1/2)*((sd_target)^2+(sd_cohort1)^2))),
                asmd_c1_c2 = round(abs(smd1), digits = 3),
                asmd_t_c1 = round(abs(smd2), digits = 3))



#Binary variable
a2 <- a %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )%>%
  dplyr::mutate(x1 = as.numeric(cohort1)/100,
         x2 = as.numeric(cohort2)/100,
         smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
         asmd_c1_c2 = round(abs(smd1), digits = 3),
         x3 = as.numeric(target)/100,
         smd2 = (x3 - x1)/(sqrt((1/2)*(x3*(1-x3)+x1*(1-x1)))),
         asmd_t_c1 = round(abs(smd2), digits = 3))

a1 <- a1 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2, asmd_t_c1)
a2 <- a2 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2, asmd_t_c1)
smd_pre_match <- rbind(a1,a2)


#create smd after matching c1-c2
b <- b %>% 
  dplyr::mutate(group_level = gsub("comparator ", "cohort", group_level)) %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Prior observation", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
         period = ifelse(period >16, period - 16, period),
         group = gsub(".*? ", "", group_level)) 

#Continuous variable
b1 <- b %>%
  dplyr::filter(estimate_type %in% c("mean", "sd")) %>%
  dplyr::mutate(estimate = as.numeric(estimate)) %>%
  pivot_wider(
    names_from = estimate_type,
    values_from = estimate
  )%>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, sd)
  )%>%
  dplyr::mutate(smd1 = (mean_cohort1 - mean_cohort2)/(sqrt((1/2)*((sd_cohort1)^2+(sd_cohort2)^2))),
                asmd_c1_c2 = round(abs(smd1), digits = 3))

#Binary variable
b2 <- b %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )%>%
  dplyr::mutate(x1 = as.numeric(cohort1)/100,
                x2 = as.numeric(cohort2)/100,
                smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
                asmd_c1_c2 = round(abs(smd1), digits = 3))

b1 <- b1 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2)
b2 <- b2 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_c1_c2)
smd_post_match_c1_c2 <- rbind(b1,b2)

exclude <- c("Fractures", "Hiv", "Hormonal contraceptives syst", "Malignant neoplastic disease", "Antineoplastic agents")
imbal_c1_c2 <- smd_post_match_c1_c2 %>%
  dplyr::filter(!(variable_level %in% exclude))%>%
  dplyr::filter(variable!="Age") %>% #already age-matched and included in PS model
  dplyr::filter(asmd_c1_c2 >0.1) %>%
  dplyr::select(variable_level)%>%
  unique()%>%
  mutate(variable_level = to_snake_case(variable_level))

#create smd after matching t-c1
c <- c %>% 
  dplyr::mutate(group_level = gsub("comparator ", "cohort", group_level)) %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Prior observation", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
         period = ifelse(period >16, period - 16, period),
         group = gsub(".*? ", "", group_level)) 

#Continuous variable
c1 <- c %>%
  dplyr::filter(estimate_type %in% c("mean", "sd")) %>%
  dplyr::mutate(estimate = as.numeric(estimate)) %>%
  pivot_wider(
    names_from = estimate_type,
    values_from = estimate
  )%>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, sd)
  )%>%
  dplyr::mutate(smd1 = (mean_target - mean_cohort1)/(sqrt((1/2)*((sd_target)^2+(sd_cohort1)^2))),
                asmd_t_c1 = round(abs(smd1), digits = 3))

#Binary variable
c2 <- c %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )%>%
  dplyr::mutate(x1 = as.numeric(target)/100,
                x2 = as.numeric(cohort1)/100,
                smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
                asmd_t_c1 = round(abs(smd1), digits = 3))

c1 <- c1 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_t_c1)
c2 <- c2 %>% 
  dplyr::select(cdm_name, variable, variable_level, period, asmd_t_c1)
smd_post_match_t_c1 <- rbind(c1,c2)

exclude <- c("Fractures", "Hiv", "Hormonal contraceptives syst", "Malignant neoplastic disease", "Antineoplastic agents")
imbal_t_c1 <- smd_post_match_t_c1 %>%
  dplyr::filter(!(variable_level %in% exclude))%>%
  dplyr::filter(variable!="Age") %>% #already age-matched and included in PS model
  dplyr::filter(asmd_t_c1 >0.1) %>%
  dplyr::select(variable_level)%>%
  unique()%>%
  mutate(variable_level = to_snake_case(variable_level))


