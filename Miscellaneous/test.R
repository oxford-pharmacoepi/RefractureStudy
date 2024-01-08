####
info(logger, "START MATCHING BETWEEN COMPARATOR COHORT 1 AND COMPARATOR COHORT 2")
lasso_reg_12 <- list()
selectedLassoFeatures12 <- list()
match_results_12 <- list()
subclasses12 <- list()
summary12 <- list()

for (l in (1:length(compCohort1))){
  set.seed(12345)
  load(here(sub_output_folder, "tempData", "subfeatures.RData"))
  subfeatures_12 <- subfeatures %>% 
    dplyr::inner_join(rbind(compCohort1[[l]] %>% dplyr::select(subject_id, index_date, group), 
                            compCohort2[[l]] %>% dplyr::select(subject_id, index_date, group)),
                      by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_12 <- lowerBoundLasso12(subfeatures_12, 50)
  
  features_lasso12 <- subfeatures_12 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_12)
  load(here(sub_output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso12 <- allSubjectsCohort %>% 
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 2", "comparator 1")) %>%
    dplyr::inner_join(features_lasso12, by = c("subject_id", "group", "cohort_start_date"="index_date"), relationship = "many-to-many")
  
  features_lasso12$prior_observation <- as.double(features_lasso12$prior_observation)
  features_lasso12 <- features_lasso12 %>% 
    dplyr::mutate(group = factor(group, c("comparator 2", "comparator 1"))) %>% 
    dplyr::rename(index_date = cohort_start_date) %>% 
    dplyr::select(-cohort_end_date, -cohort_definition_id)
  
  x <- data.matrix(features_lasso12 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso12$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_12[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_12[[l]], s = lasso_reg_12[[l]]$lambda.1se)
  selectedLassoFeatures12[[l]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures12[[l]] <- selectedLassoFeatures12[[l]][selectedLassoFeatures12[[l]] != "(Intercept)"]
  selectedLassoFeatures12[[l]] <- c("age", selectedLassoFeatures12[[l]]) %>% unique()
  
  features_lasso12 <- features_lasso12 %>%
    dplyr::select(all_of(c("subject_id", "group", "index_date", "follow_up_end", selectedLassoFeatures12[[l]])))
  
  match_results_12[[l]] <- matchit(group ~ .-subject_id -index_date -follow_up_end, 
                                   data=features_lasso12,
                                   antiexact = ~subject_id,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  subclasses12[[l]] <- match.data(match_results_12[[l]])
  summary12[[l]] <- summary(match_results_12[[l]])
  rm(allSubjectsCohort, features_lasso12)
}

save(lasso_reg_12, file = here(sub_output_folder, "tempData2", "lasso_reg_12.RData"))
rm(lasso_reg_12)
save(selectedLassoFeatures12, file = here(sub_output_folder, "tempData2", "selectedLassoFeatures12.RData"))
rm(selectedLassoFeatures12)
save(match_results_12, file = here(sub_output_folder, "tempData2", "match_results_12.RData"))
rm(match_results_12)
save(subclasses12, file = here(sub_output_folder, "tempData2", "subclasses12.RData"))
rm(subclasses12)
save(summary12, file = here(sub_output_folder, "tempData2", "summary12.RData"))
rm(summary12)
gc()

####
lasso_reg_12_15 <- list()
selectedLassoFeatures12_15 <- list()
match_results_12_15 <- list()
subclasses12_15 <- list()
summary12_15 <- list()

for (l in (1:length(compCohort1))){
  set.seed(12345)
  load(here(sub_output_folder, "tempData", "subfeatures.RData"))
  subfeatures_12 <- subfeatures %>% 
    dplyr::inner_join(rbind(compCohort1[[l]] %>% dplyr::select(subject_id, index_date, group), 
                            compCohort2_15[[l]] %>% dplyr::select(subject_id, index_date, group)),
                      by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_12 <- lowerBoundLasso12(subfeatures_12, 50)
  
  features_lasso12 <- subfeatures_12 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_12)
  load(here(sub_output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso12 <- allSubjectsCohort %>% 
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 2", "comparator 1")) %>%
    dplyr::inner_join(features_lasso12, by = c("subject_id", "group", "cohort_start_date"="index_date"), relationship = "many-to-many")
  
  features_lasso12$prior_observation <- as.double(features_lasso12$prior_observation)
  features_lasso12 <- features_lasso12 %>% 
    dplyr::mutate(group = factor(group, c("comparator 2", "comparator 1"))) %>% 
    dplyr::rename(index_date = cohort_start_date) %>% 
    dplyr::select(-cohort_end_date, -cohort_definition_id)
  
  x <- data.matrix(features_lasso12 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso12$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_12_15[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_12_15[[l]], s = lasso_reg_12_15[[l]]$lambda.1se)
  selectedLassoFeatures12_15[[l]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures12_15[[l]] <- selectedLassoFeatures12_15[[l]][selectedLassoFeatures12_15[[l]] != "(Intercept)"]
  selectedLassoFeatures12_15[[l]] <- c("age", selectedLassoFeatures12_15[[l]]) %>% unique()
  
  features_lasso12 <- features_lasso12 %>%
    dplyr::select(all_of(c("subject_id", "group", "index_date", "follow_up_end", selectedLassoFeatures12_15[[l]])))
  
  match_results_12_15[[l]] <- matchit(group ~ .-subject_id -index_date -follow_up_end, 
                                   data=features_lasso12,
                                   antiexact = ~subject_id,
                                   method="nearest", 
                                   caliper= c(0.20, age = 5), 
                                   std.caliper = c(TRUE, FALSE),
                                   ratio=5)
  subclasses12_15[[l]] <- match.data(match_results_12_15[[l]])
  summary12_15[[l]] <- summary(match_results_12_15[[l]])
  rm(allSubjectsCohort, features_lasso12)
}

save(lasso_reg_12_15, file = here(sub_output_folder, "tempData", "lasso_reg_12_15.RData"))
rm(lasso_reg_12_15)
save(selectedLassoFeatures12_15, file = here(sub_output_folder, "tempData", "selectedLassoFeatures12_15.RData"))
rm(selectedLassoFeatures12_15)
save(match_results_12_15, file = here(sub_output_folder, "tempData", "match_results_12_15.RData"))
rm(match_results_12_15)
save(subclasses12_15, file = here(sub_output_folder, "tempData", "subclasses12_15.RData"))
rm(subclasses12_15)
save(summary12_15, file = here(sub_output_folder, "tempData", "summary12_15.RData"))
rm(summary12_15)
gc()

####
lasso_reg_12_20 <- list()
selectedLassoFeatures12_20 <- list()
match_results_12_20 <- list()
subclasses12_20 <- list()
summary12_20 <- list()

for (l in (1:length(compCohort1))){
  set.seed(12345)
  load(here(sub_output_folder, "tempData", "subfeatures.RData"))
  subfeatures_12 <- subfeatures %>% 
    dplyr::inner_join(rbind(compCohort1[[l]] %>% dplyr::select(subject_id, index_date, group), 
                            compCohort2_20[[l]] %>% dplyr::select(subject_id, index_date, group)),
                      by = c("subject_id", "index_date"))
  rm(subfeatures)
  
  subfeatures_12 <- lowerBoundLasso12(subfeatures_12, 50)
  
  features_lasso12 <- subfeatures_12 %>% 
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    pivot_wider(names_from = "feature", values_from =  "value", values_fill = 0)
  
  rm(subfeatures_12)
  load(here(sub_output_folder, "tempData", "allSubjectsCohort.RData"))
  
  features_lasso12 <- allSubjectsCohort %>% 
    dplyr::filter(period == l) %>%
    dplyr::select(-"period") %>%
    dplyr::filter(group %in% c("comparator 2", "comparator 1")) %>%
    dplyr::inner_join(features_lasso12, by = c("subject_id", "group", "cohort_start_date"="index_date"), relationship = "many-to-many")
  
  features_lasso12$prior_observation <- as.double(features_lasso12$prior_observation)
  features_lasso12 <- features_lasso12 %>% 
    dplyr::mutate(group = factor(group, c("comparator 2", "comparator 1"))) %>% 
    dplyr::rename(index_date = cohort_start_date) %>% 
    dplyr::select(-cohort_end_date, -cohort_definition_id)
  
  x <- data.matrix(features_lasso12 %>% dplyr::select(-c("group", "subject_id", "index_date", "follow_up_end")))
  y <- features_lasso12$group
  y<-as.integer(y)
  lambdas <- 10^seq(2, -3, by = -.1)
  lasso_reg_12_20[[l]] <- cv.glmnet(x, y, lambda = lambdas, standardize = TRUE, nfolds = 5, family = "binomial", alpha = 1)
  rm(x,y)
  
  coef.lasso_reg <- coef(lasso_reg_12_20[[l]], s = lasso_reg_12_20[[l]]$lambda.1se)
  selectedLassoFeatures12_20[[l]] <- names(coef.lasso_reg[(coef.lasso_reg[,1]!=0),1])
  selectedLassoFeatures12_20[[l]] <- selectedLassoFeatures12_20[[l]][selectedLassoFeatures12_20[[l]] != "(Intercept)"]
  selectedLassoFeatures12_20[[l]] <- c("age", selectedLassoFeatures12_20[[l]]) %>% unique()
  
  features_lasso12 <- features_lasso12 %>%
    dplyr::select(all_of(c("subject_id", "group", "index_date", "follow_up_end", selectedLassoFeatures12_20[[l]])))
  
  match_results_12_20[[l]] <- matchit(group ~ .-subject_id -index_date -follow_up_end, 
                                      data=features_lasso12,
                                      antiexact = ~subject_id,
                                      method="nearest", 
                                      caliper= c(0.20, age = 5), 
                                      std.caliper = c(TRUE, FALSE),
                                      ratio=5)
  subclasses12_20[[l]] <- match.data(match_results_12_20[[l]])
  summary12_20[[l]] <- summary(match_results_12_20[[l]])
  rm(allSubjectsCohort, features_lasso12)
}

save(lasso_reg_12_20, file = here(sub_output_folder, "tempData", "lasso_reg_12_20.RData"))
rm(lasso_reg_12_20)
save(selectedLassoFeatures12_20, file = here(sub_output_folder, "tempData", "selectedLassoFeatures12_20.RData"))
rm(selectedLassoFeatures12_20)
save(match_results_12_20, file = here(sub_output_folder, "tempData", "match_results_12_20.RData"))
rm(match_results_12_20)
save(subclasses12_20, file = here(sub_output_folder, "tempData", "subclasses12_20.RData"))
rm(subclasses12_20)
save(summary12_20, file = here(sub_output_folder, "tempData", "summary12_20.RData"))
rm(summary12_20)
gc()


####table1s
info(logger, "START TO CREATE THE SUMMARY - BEFORE MATCHING")
cdm[["before_matching_test"]] <- 
  compCohort1[[1]] %>%
  dplyr::select(subject_id, index_date, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::mutate(cohort_name = "comparator cohort 1") %>%
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::union_all(compCohort2[[1]] %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "comparator cohort 2 unsampled") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 2)) %>%
  dplyr::union_all(compCohort2_15[[1]] %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "comparator cohort 2 sampled by 15") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 3)) %>%
  dplyr::union_all(compCohort2_20[[1]] %>%
                     dplyr::select(subject_id, index_date, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::mutate(cohort_name = "comparator cohort 2 sampled by 20") %>%
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = 4)) %>%
  dplyr::select(-period) %>% 
  computeQuery(
    name = "before_matching_test", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "before_matching_test")
dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(schema = attr(cdm, "write_schema"), table = "before_matching_test"), cdm[["before_matching_test"]], overwrite = T)
cdm[["before_matching_test"]] <- dplyr::tbl(attr(cdm,"dbcon"), fullName)

cdm[["table_one_test_cohort"]] <- cdm[["before_matching_test"]] %>%
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>%
  computeQuery(
    name = "table_one_test_cohort",
    temporary = FALSE,
    schema = attr(cdm, "write_schema"),
    overwrite = TRUE
  )

table_one_test_cohort_set <- cdm[["before_matching_test"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "table_one_test_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
table_one_test_cohort_count <- cdm[["before_matching_test"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "table_one_test_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char[["table_one_test_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["table_one_test_cohort"]],
                                                        cohortSetRef = table_one_test_cohort_set,
                                                        cohortCountRef = table_one_test_cohort_count,
                                                        overwrite = T)

cdm_char <- CDMConnector::cdmSubsetCohort(cdm_char, "table_one_test_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char)
cdm_char <- generateDrugUtilisationCohortSet(cdm = cdm_char, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char)
cdm_char <- generateConceptCohortSet(cdm = cdm_char, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_before_matching_test <- cdm_char[["table_one_test_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_before_matching_test, here(t1_sub_output_folder, "result_before_matching_test.csv"))

reformat_table_one_sample<- function(table_one, selected_grp, name1, name2){
  group_level_selected <- c("Comparator cohort 1",
                            paste0("Comparator cohort 2 ", selected_grp))
  
  sub_table_one <- table_one %>% 
    dplyr::filter(group_level %in% group_level_selected)
  
  sub_table_one_list <- list()
  for (i in (1:2)){
    sub_table_one_list[[i]]<-sub_table_one %>% 
      dplyr::filter(group_level == group_level_selected[[i]])
  }
  table_one_1 <- sub_table_one_list[[1]]
  table_one_2 <- sub_table_one_list[[2]]
  reformatted_table1 <- data.frame(x = NA, y= NA, z = NA, mean1 = NA, sd1 = NA, mean2 = NA, sd2 = NA)
  n1 <- table_one_1 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  n2 <- table_one_2 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  # variables assembled by mean
  cont_var <- table_one_1 %>% dplyr::filter(estimate_type == "mean") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cont_var))){
    reformatted_table1 <- rbind(reformatted_table1, 
                                data.frame(x = paste0(cont_var[[i]], ", mean (SD)"), 
                                           y = paste0(round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 1),
                                                      " (",
                                                      round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 1),
                                                      ")"),
                                           z = paste0(round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 1),
                                                      " (",
                                                      round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 1),
                                                      ")"),
                                           mean1 = table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate),
                                           sd1 = table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate),
                                           mean2 = table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate),
                                           sd2 = table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)
                                )
    )
  }
  reformatted_table1 <- reformatted_table1 %>% 
    dplyr::mutate(smd = (as.numeric(mean1)-as.numeric(mean2))/(sqrt(1/2*(as.numeric(sd1)^2+as.numeric(sd2)^2)))) %>% 
    dplyr::select(x,y,z,smd)
  
  reformatted_table1 <- reformatted_table1[-1, ]
  
  #variables assembled by percentage
  cont_var <- table_one_1 %>% 
    dplyr::filter(estimate_type == "percentage") %>% 
    filter(!variable == "Sex") %>% 
    dplyr::select(variable_level) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(variable_level)  
  
  reformatted_table1_2<-data.frame(x = NA, y= NA, z= NA, percentage1 = NA, percentage2 = NA)
  for (i in (1:length(cont_var))){
    reformatted_table1_2 <- rbind(reformatted_table1_2, data.frame(x = paste0(cont_var[[i]], ", n(%)"),
                                                                   y = paste0(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                              " (",
                                                                              round(as.numeric(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                              ")"),
                                                                   z = paste0(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                              " (",
                                                                              round(as.numeric(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                              ")"),
                                                                   percentage1 = (1/100)*as.numeric(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)),
                                                                   percentage2 = (1/100)*as.numeric(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate))))
  }
  
  reformatted_table1_2 <- reformatted_table1_2 %>% 
    dplyr::mutate(smd = (percentage1-percentage2)/(sqrt((1/2)*(percentage1*(1-percentage1)+percentage2*(1-percentage2))))) %>% 
    dplyr::select(x,y,z, smd)
  
  reformatted_table1_2 <- reformatted_table1_2[-1, ]
  
  reformatted_table1 <- rbind(reformatted_table1, reformatted_table1_2)
  reformatted_table1 <- reformatted_table1 %>% 
    dplyr::mutate(smd = round(abs(smd), digits = 3))
  ###rename columns
  colnames(reformatted_table1) <- c(
    "Characteristic",
    paste0(name1, " (n = ", n1, ")"),
    paste0(name2, " (n = ", n2, ")"),
    "SMD"
  )
  return(reformatted_table1)
}

  output<-reformat_table_one_sample(result_before_matching_test, selected_grp =  "unsampled", name1 = "C1", name2 = "C2 unsampled") %>%
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_period_1_before_matching_unsampled.csv")))

  output<-reformat_table_one_sample(result_before_matching_test, selected_grp =  "sampled by 15", name1 = "C1", name2 = "C2 sampled by 15") %>%
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_period_1_before_matching_sampled_15.csv")))
  
  output<-reformat_table_one_sample(result_before_matching_test, selected_grp =  "sampled by 20", name1 = "C1", name2 = "C2 sampled by 20") %>%
    dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
  write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_period_1_before_matching_sampled_20.csv")))

########################## after matching 12 ####################
##12 unsampled
  
  reformat_table_one_sample2<- function(table_one, name1, name2){
    group_level_selected <- c("1 comparator 1",
                              "17 comparator 2")
    
    sub_table_one <- table_one %>% 
      dplyr::filter(group_level %in% group_level_selected)
    
    sub_table_one_list <- list()
    for (i in (1:2)){
      sub_table_one_list[[i]]<-sub_table_one %>% 
        dplyr::filter(group_level == group_level_selected[[i]])
    }
    table_one_1 <- sub_table_one_list[[1]]
    table_one_2 <- sub_table_one_list[[2]]
    reformatted_table1 <- data.frame(x = NA, y= NA, z = NA, mean1 = NA, sd1 = NA, mean2 = NA, sd2 = NA)
    n1 <- table_one_1 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
    n2 <- table_one_2 %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
    
    # variables assembled by mean
    cont_var <- table_one_1 %>% dplyr::filter(estimate_type == "mean") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
    
    for (i in (1:length(cont_var))){
      reformatted_table1 <- rbind(reformatted_table1, 
                                  data.frame(x = paste0(cont_var[[i]], ", mean (SD)"), 
                                             y = paste0(round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 1),
                                                        " (",
                                                        round(as.numeric(table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 1),
                                                        ")"),
                                             z = paste0(round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate)), digits = 1),
                                                        " (",
                                                        round(as.numeric(table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)), digits = 1),
                                                        ")"),
                                             mean1 = table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate),
                                             sd1 = table_one_1 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate),
                                             mean2 = table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "mean") %>% dplyr::pull(estimate),
                                             sd2 = table_one_2 %>% dplyr::filter(variable == cont_var[[i]]) %>% dplyr::filter(estimate_type == "sd") %>% dplyr::pull(estimate)
                                  )
      )
    }
    reformatted_table1 <- reformatted_table1 %>% 
      dplyr::mutate(smd = (as.numeric(mean1)-as.numeric(mean2))/(sqrt(1/2*(as.numeric(sd1)^2+as.numeric(sd2)^2)))) %>% 
      dplyr::select(x,y,z,smd)
    
    reformatted_table1 <- reformatted_table1[-1, ]
    
    #variables assembled by percentage
    cont_var <- table_one_1 %>% 
      dplyr::filter(estimate_type == "percentage") %>% 
      filter(!variable == "Sex") %>% 
      dplyr::select(variable_level) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(variable_level)  
    
    reformatted_table1_2<-data.frame(x = NA, y= NA, z= NA, percentage1 = NA, percentage2 = NA)
    for (i in (1:length(cont_var))){
      reformatted_table1_2 <- rbind(reformatted_table1_2, data.frame(x = paste0(cont_var[[i]], ", n(%)"),
                                                                     y = paste0(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                " (",
                                                                                round(as.numeric(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                ")"),
                                                                     z = paste0(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                                " (",
                                                                                round(as.numeric(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                                ")"),
                                                                     percentage1 = (1/100)*as.numeric(table_one_1 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)),
                                                                     percentage2 = (1/100)*as.numeric(table_one_2 %>% dplyr::filter(variable_level == cont_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate))))
    }
    
    reformatted_table1_2 <- reformatted_table1_2 %>% 
      dplyr::mutate(smd = (percentage1-percentage2)/(sqrt((1/2)*(percentage1*(1-percentage1)+percentage2*(1-percentage2))))) %>% 
      dplyr::select(x,y,z, smd)
    
    reformatted_table1_2 <- reformatted_table1_2[-1, ]
    
    reformatted_table1 <- rbind(reformatted_table1, reformatted_table1_2)
    reformatted_table1 <- reformatted_table1 %>% 
      dplyr::mutate(smd = round(abs(smd), digits = 3))
    ###rename columns
    colnames(reformatted_table1) <- c(
      "Characteristic",
      paste0(name1, " (n = ", n1, ")"),
      paste0(name2, " (n = ", n2, ")"),
      "SMD"
    )
    return(reformatted_table1)
  }
  
load(here::here(sub_output_folder, "tempData2", "subclasses12.RData"))
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 12")
subclasses12_table1 <- subclasses12
for (i in (1:length(subclasses12))){
  subclasses12_table1[[i]] <- subclasses12[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_c1c2"]] <- 
  Reduce(dplyr::union_all, subclasses12_table1) %>%
  dplyr::select(subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
  dplyr::select(-group, -period) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses12_table1) %>%
                     dplyr::select(subject_id, index_date, group, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 2") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+period) %>%
                     dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
                     dplyr::select(-group, -period) %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_c1c2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_c1c2")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_c1c2"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_c1c2"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_12_cohort"]] <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_12_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_12_cohort_set <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_12_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_12_cohort_count <- cdm[["after_matching_c1c2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_12_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char2 <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char2[["after_matching_12_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort"]],
                                                                 cohortSetRef = after_matching_12_cohort_set,
                                                                 cohortCountRef = after_matching_12_cohort_count,
                                                                 overwrite = T)

cdm_char2 <- CDMConnector::cdmSubsetCohort(cdm_char2, "after_matching_12_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char2)
cdm_char2 <- generateDrugUtilisationCohortSet(cdm = cdm_char2, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char2)
cdm_char2 <- generateConceptCohortSet(cdm = cdm_char2, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_after_matching12 <- cdm_char2[["after_matching_12_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_after_matching12, here(t1_sub_output_folder, "result_after_matching12_unsampled.csv"))

output<-reformat_table_one_sample2(result_after_matching12, name1 = "C1", name2 = "C2 unsampled") %>%
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_period_1_after_matching_unsampled.csv")))

##12 - sample by 20
load(here::here(sub_output_folder, "tempData", "subclasses12_20.RData"))
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 12")
subclasses12_table1 <- subclasses12_20
for (i in (1:length(subclasses12_20))){
  subclasses12_table1[[i]] <- subclasses12_20[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_c1c2"]] <- 
  Reduce(dplyr::union_all, subclasses12_table1) %>%
  dplyr::select(subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
  dplyr::select(-group, -period) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses12_table1) %>%
                     dplyr::select(subject_id, index_date, group, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 2") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+period) %>%
                     dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
                     dplyr::select(-group, -period) %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_c1c2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_c1c2")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_c1c2"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_c1c2"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_12_cohort"]] <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_12_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_12_cohort_set <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_12_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_12_cohort_count <- cdm[["after_matching_c1c2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_12_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char2 <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char2[["after_matching_12_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort"]],
                                                                 cohortSetRef = after_matching_12_cohort_set,
                                                                 cohortCountRef = after_matching_12_cohort_count,
                                                                 overwrite = T)

cdm_char2 <- CDMConnector::cdmSubsetCohort(cdm_char2, "after_matching_12_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char2)
cdm_char2 <- generateDrugUtilisationCohortSet(cdm = cdm_char2, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char2)
cdm_char2 <- generateConceptCohortSet(cdm = cdm_char2, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_after_matching12 <- cdm_char2[["after_matching_12_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_after_matching12, here(t1_sub_output_folder, "result_after_matching12_sampled_20.csv"))

output<-reformat_table_one_sample2(result_after_matching12, name1 = "C1", name2 = "C2 sampled by 20") %>%
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_period_1_after_matching_sampled_20.csv")))

##12 - sample by 15
load(here::here(sub_output_folder, "tempData", "subclasses12_15.RData"))
info(logger, "START TO CREATE THE SUMMARY - AFTER MATCHING 12")
subclasses12_table1 <- subclasses12_15
for (i in (1:length(subclasses12_15))){
  subclasses12_table1[[i]] <- subclasses12_15[[i]] %>%
    dplyr::select(subject_id, group, index_date) %>% 
    dplyr::mutate(period = i)
}

cdm[["after_matching_c1c2"]] <- 
  Reduce(dplyr::union_all, subclasses12_table1) %>%
  dplyr::select(subject_id, index_date, group, period) %>% 
  dplyr::rename(cohort_start_date = index_date) %>% 
  dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
  dplyr::filter(group == "comparator 1") %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(cohort_definition_id = period) %>%
  dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
  dplyr::select(-group, -period) %>% 
  dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id) %>% 
  dplyr::union_all(Reduce(dplyr::union_all, subclasses12_table1) %>%
                     dplyr::select(subject_id, index_date, group, period) %>% 
                     dplyr::rename(cohort_start_date = index_date) %>% 
                     dplyr::mutate(cohort_end_date = cohort_start_date) %>% 
                     dplyr::filter(group == "comparator 2") %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(cohort_definition_id = tot_periods_target+period) %>%
                     dplyr::mutate(cohort_name = paste0(cohort_definition_id, "_", group)) %>% 
                     dplyr::select(-group, -period) %>% 
                     dplyr::select(subject_id, cohort_start_date, cohort_end_date, cohort_name, cohort_definition_id)) %>% 
  computeQuery(
    name = "after_matching_c1c2", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

fullName <- inSchema(attr(cdm, "write_schema"), "after_matching_c1c2")
dbWriteTable(attr(cdm, "dbcon"), fullName, cdm[["after_matching_c1c2"]] |> as_tibble(), overwrite = T)
cdm[["after_matching_c1c2"]] <- dplyr::tbl(attr(cdm , "dbcon"), fullName)

cdm[["after_matching_12_cohort"]] <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select(-cohort_name) %>% 
  dplyr::select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) %>% 
  computeQuery(
    name = "after_matching_12_cohort", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

after_matching_12_cohort_set <- cdm[["after_matching_c1c2"]] %>% 
  dplyr::select("cohort_definition_id", "cohort_name") %>% 
  dplyr::distinct() %>% 
  computeQuery(
    name = "after_matching_12_cohort_set", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

#cohort count
after_matching_12_cohort_count <- cdm[["after_matching_c1c2"]] %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::tally() %>%
  dplyr::compute() %>%
  dplyr::rename(number_records = n) %>%
  dplyr::mutate(number_subjects = number_records) %>%
  computeQuery(
    name = "after_matching_12_cohort_count", 
    temporary = FALSE, 
    schema = attr(cdm, "write_schema"), 
    overwrite = TRUE
  )

stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")
cdm_char2 <-CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema
)

cdm_char2[["after_matching_12_cohort"]] <- newGeneratedCohortSet(cohortRef = cdm[["after_matching_12_cohort"]],
                                                                 cohortSetRef = after_matching_12_cohort_set,
                                                                 cohortCountRef = after_matching_12_cohort_count,
                                                                 overwrite = T)

cdm_char2 <- CDMConnector::cdmSubsetCohort(cdm_char2, "after_matching_12_cohort", verbose = T)

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS - BEFORE MATCHING")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm_char2)
cdm_char2 <- generateDrugUtilisationCohortSet(cdm = cdm_char2, name = medications, conceptSet = codelistMedications)

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS - BEFORE MATCHING")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm_char2)
cdm_char2 <- generateConceptCohortSet(cdm = cdm_char2, name = conditions, conceptSet = codelistConditions, overwrite = T)

# create table summary
info(logger, "CREATE SUMMARY - BEFORE MATCHING")
result_after_matching12 <- cdm_char2[["after_matching_12_cohort"]] %>%
  summariseCharacteristics(
    ageGroup = list(c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 99), c(100,150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      ),
      "Medications" = list(
        tableName = "drug_era", value = "count", window = c(-365, 0)
      )
    ), 
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = medications, value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = conditions, value = "flag", window = c(-Inf, 0)
      )
    )
  )
write_csv(result_after_matching12, here(t1_sub_output_folder, "result_after_matching12_sampled_15.csv"))

output<-reformat_table_one_sample2(result_after_matching12, name1 = "C1", name2 = "C2 sampled 15") %>%
  dplyr::filter(!Characteristic %in% c("Fractures, n(%)", "Malignant neoplastic disease, n(%)"))
write_csv(output, here(t1_sub_output_folder, paste0("c1_c2_period_1_after_matching_sampled_15.csv")))
