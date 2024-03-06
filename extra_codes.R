#####
a <- result_before_matching_v2
b <- result_after_matching12_v2
c <- result_after_matching01_v2

#create smd before matching
#unequal number of rows per period caused by small numbers in age group 100-150#
a <- a %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
         period = ifelse(period >16, period - 16, period),
         group = gsub(".*? ", "", group_level)) 
a <- a %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )

a <- a %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::mutate(x1 = as.numeric(cohort1)/100,
         x2 = as.numeric(cohort2)/100,
         smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
         asmd_c1_c2 = round(abs(smd1), digits = 3),
         x3 = as.numeric(target)/100,
         smd2 = (x3 - x1)/(sqrt((1/2)*(x3*(1-x3)+x1*(1-x1)))),
         asmd_t_c1 = round(abs(smd2), digits = 3))

#create smd after matching c1-c2
b <- b %>% 
  dplyr::mutate(group_level = gsub("comparator ", "cohort", group_level))
         
b <- b %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
         period = ifelse(period >16, period - 16, period),
         group = gsub(".*? ", "", group_level)) 
b <- b %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )

b <- b %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::mutate(x1 = as.numeric(cohort1)/100,
         x2 = as.numeric(cohort2)/100,
         smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
         asmd_c1_c2 = round(abs(smd1), digits = 3))

exclude <- c("Fractures", "Hiv", "Hormonal contraceptives syst", "Malignant neoplastic disease", "Antineoplastic agents")
imbal_c1_c2 <- b %>%
  dplyr::filter(!(variable_level %in% exclude))%>%
  dplyr::filter(asmd_c1_c2 >0.1) %>%
  dplyr::select(variable_level)%>%
  unique() 

#create smd after matching t-c1
c <- c %>% 
  dplyr::mutate(group_level = gsub("comparator ", "cohort", group_level))

c <- c %>%
  dplyr::filter(!variable %in% c("Age group", "Sex", "Cohort start date", "Cohort end date", "Future observation")) %>%
  dplyr::mutate(period = as.numeric(gsub( " .*$", "", group_level)),
         period = ifelse(period >16, period - 16, period),
         group = gsub(".*? ", "", group_level)) 
c <- c %>%
  dplyr::select(-group_level)%>%
  pivot_wider(
    names_from = group,
    values_from = estimate
  )

c <- c %>%
  dplyr::filter(estimate_type == "percentage") %>%
  dplyr::mutate(x1 = as.numeric(target)/100,
         x2 = as.numeric(cohort1)/100,
         smd1 = (x1 - x2)/(sqrt((1/2)*(x1*(1-x1)+x2*(1-x2)))),
         asmd_t_c1 = round(abs(smd1), digits = 3))

imbal_t_c1 <- c %>%
  dplyr::filter(!(variable_level %in% exclude))%>%
  dplyr::filter(asmd_t_c1 >0.1) %>%
  dplyr::select(variable_level)%>%
  unique() 
