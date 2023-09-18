plotFolder <- here(output_folder, "plots")
if (!dir.exists(plotFolder)) {
  dir.create(plotFolder)
}

# Creating follow up time
info(logger, "CREATING FOLLOW UP TIME: FOLLOWUPEND")
fracture_table_follow_up <- fracture_table_rq2

# 730 days after the index date
fracture_table_follow_up <- addInTwoYearsAfter(fracture_table_follow_up)

# End of data collection (assuming each person has only one observation period)
fracture_table_follow_up <- addInObsEndDate(fracture_table_follow_up)

# Adding first cancer date after the index date
fracture_table_follow_up <- addInCancerPostIndex(fracture_table_follow_up)

# Adding first bone disease date after the index date
fracture_table_follow_up <- addInBoneDiseasePostIndex(fracture_table_follow_up)
  
# Add in first fracture date after the index dates
fracture_table_follow_up <- addInNextFracture(fracture_table_follow_up)

# Add in death date after the index date 
fracture_table_follow_up <- addInDeath(fracture_table_follow_up)

# Add in FOLLOWUPEND
fracture_table_follow_up <- addInFollowUpEnd(fracture_table_follow_up)

# Add in immFracture
fracture_table_follow_up <- immFracture(fracture_table_follow_up)

fracture_table_follow_up_back_up <- fracture_table_follow_up

reverseEntryTable <- list()

while (nrow(fracture_table_follow_up_back_up) > 0){
  reverseEntryTable[[nrow(fracture_table_follow_up_back_up)]] <- fracture_table_follow_up_back_up %>% filter(follow_up_time > 0)
  fracture_table_follow_up_back_up <- fracture_table_follow_up_back_up %>% filter(follow_up_time > 0)
  fracture_table_follow_up_back_up <- nextFractureClean(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- noDeathOnIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- noCancerPriorOrOnIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- noBoneDiseasePriorOrOnIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInTwoYearsAfter(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInObsEndDate(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInCancerPostIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInBoneDiseasePostIndex(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInNextFracture(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInDeath(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- addInFollowUpEnd(fracture_table_follow_up_back_up)
  fracture_table_follow_up_back_up <- fracture_table_follow_up_back_up %>% ungroup()
  fracture_table_follow_up_back_up <- immFracture(fracture_table_follow_up_back_up)
}

reverseEntryTable <- reverseEntryTable[!sapply(reverseEntryTable, is.null)]

#entryTable serves as a table recording people who entered the cohort at least once, twice etc..
entryTable <- list()
for (i in (1:length(reverseEntryTable))){
  entryTable[[i]]<-reverseEntryTable[[length(reverseEntryTable)+1-i]]
}

ObservedTime <- list()

for (i in (1:length(entryTable))){
  ObservedTime[[i]] <- entryTable[[i]] %>% 
    select(subject_id, follow_up_time) %>% 
    distinct() %>% 
    summarise(total_time = sum(follow_up_time))
}

totalObservedTime <- 0
for (i in (1:length(ObservedTime))){
  totalObservedTime <- totalObservedTime + ObservedTime[[i]]
}

totalObservedTimeYears <- as.integer(totalObservedTime)/365.25

totalFracture <- 0
for (i in (1:length(reverseEntryTable))){
  totalFracture <- totalFracture + sum(entryTable[[i]]$imminentFracture)
}

inc_results <- tibble(fracture = totalFracture, 
                      py = totalObservedTimeYears)

confidenceInterval <- PoissonCI(x=totalFracture, n=totalObservedTimeYears/1000, method = "exact")
confidenceInterval <- as.data.frame(confidenceInterval) %>% mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% select(c(-lwr.ci, -upr.ci))
inc_results <- cbind(inc_results, tibble(n_person = entryTable[[1]] %>% distinct(subject_id) %>% tally() %>% pull()), confidenceInterval, tibble(database_name=db_name)) 

### Stratifying by the number of times someone enters the cohort
stratifiedCohort <- list()
for (i in (1:(length(entryTable)-1))){
  stratifiedCohort[[i]] <- entryTable[[i]] %>% anti_join(entryTable[[i+1]], by = "subject_id")
}

stratifiedCohort[[length(entryTable)]] <- entryTable[[length(entryTable)]]

stratifiedTime <- c()
for (i in (1: length(stratifiedCohort))){
  stratifiedTime[[i]] <- stratifiedCohort[[i]] %>% 
    select(subject_id, follow_up_time) %>% 
    distinct() %>% 
    summarise(total_time = sum(follow_up_time+730*(i-1)))
  stratifiedTime[[i]]<-as.integer(stratifiedTime[[i]] %>% pull())/365.25
}

stratifiedFractures <- c()
for (i in (1:length(stratifiedCohort))){
  stratifiedFractures[[i]] <- sum(stratifiedCohort[[i]]$imminentFracture)
}

stratifiedIncidenceResults <- list()
for (i in (1:length(stratifiedCohort))){
  stratifiedIncidenceResults[[i]] <- tibble(fracture = stratifiedFractures[[i]], py = stratifiedTime[[i]], tibble(n_person = stratifiedCohort[[i]] %>% distinct(subject_id) %>% tally() %>% pull()))
  stratifiedIncidenceResults[[i]] <- cbind(as.data.frame(PoissonCI(x=stratifiedFractures[[i]], n=stratifiedTime[[i]]/1000, method = "exact")) %>% mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% select(c(-lwr.ci, -upr.ci)), stratifiedIncidenceResults[[i]], tibble(database_name=db_name), tibble(cohort_entering_counter = i))
}

stratifiedIncidenceResultsTable <- data.frame()
for (i in (1:length(stratifiedIncidenceResults))){
  stratifiedIncidenceResultsTable<-rbind(stratifiedIncidenceResultsTable, stratifiedIncidenceResults[[i]])
}

# obscuring according to min cell count
stratifiedIncidenceResultsTable <- stratifiedIncidenceResultsTable %>% 
  mutate(obscured = n_person < minimum_counts)

stratifiedIncidenceResultsTable <- stratifiedIncidenceResultsTable %>%
  mutate(est = ifelse(obscured == T, NA, est),
         lower = ifelse(obscured == T, NA, lower),
         upper = ifelse(obscured == T, NA, upper),
         fracture = ifelse(obscured == T, NA, fracture),
         py = ifelse(obscured == T, NA, py),
         n_person = ifelse(obscured == T, NA, n_person))

# Export Incidence Results
write.xlsx(inc_results, file = here::here(output_folder, "IncidenceResults.xlsx"))
write.xlsx(stratifiedIncidenceResultsTable, file = here::here(output_folder, "StratifiedIncidenceResults.xlsx"))
save(fracture_table, file = here(output_folder, "fracture_table.RData"))

rm(inc_results, 
   reverseEntryTable, 
   stratifiedIncidenceResultsTable,
   AttritionReportRQ2,
   confidenceInterval,
   fracture_correction_nonspecific,
   fracture_correction,
   fracture_table_rq2,
   totalObservedTime,
   ObservedTime,
   stratifiedIncidenceResults,
   stratifiedTime,
   stratifiedFractures,
   fracture_table_back_up,
   fracture_table_follow_up,
   fracture_table_follow_up_back_up)

### cumulative incidence function
#no strat
cif_data_no_strat <- data.frame()

for (i in (1:length(stratifiedCohort))){
  cif_data_no_strat <- rbind(cif_data_no_strat, 
    stratifiedCohort[[i]] %>%
    mutate(follow_up_time = follow_up_time + (i-1)*730) %>%
    select(subject_id, follow_up_time) %>%
    distinct() %>%
    mutate(follow_up_time = as.integer(follow_up_time)/365.25))
}

censor_by_imminent <- data.frame()
for (i in (1: length(stratifiedCohort))){
  censor_by_imminent<-
    rbind(censor_by_imminent, 
          stratifiedCohort[[i]] %>%
    group_by(subject_id) %>%
    summarise(status = sum(condition_start_date == follow_up_end)) %>%
    filter(status==1)
    )
}

censor_by_death <- data.frame()
for (i in (1: length(stratifiedCohort))){
  censor_by_death<-
    rbind(censor_by_death, 
          stratifiedCohort[[i]] %>%
            group_by(subject_id) %>%
            summarise(status = sum(death_date == follow_up_end)) %>%
            filter(status > 0) %>%
            select(-status) %>%
            mutate(status = 2)
      
    )
}

cif_data_no_strat <-
  cif_data_no_strat %>%
  left_join(censor_by_imminent, by = "subject_id")

cif_data_no_strat2 <- cif_data_no_strat %>%
  filter(is.na(status)) %>%
  select(-status) %>%
  left_join(censor_by_death, by = "subject_id") %>%
  mutate(status = case_when(is.na(status) ~ 0,
                   status == 2 ~ 2))

cif_data_no_strat <- rbind(cif_data_no_strat %>% filter(!is.na(status)), cif_data_no_strat2)
rm(cif_data_no_strat2)

cif_data_no_strat <- cif_data_no_strat %>%
  mutate(status = case_when(status == 1 ~ "imminent",
                            status == 2 ~ "death", 
                            status == 0 ~ "censor"))

cif_data_no_strat$status <- factor(cif_data_no_strat$status, levels = c("censor", "imminent", "death"))

fit_no_strat <- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ 1, cif_data_no_strat)
save(fit_no_strat, file = here::here(output_folder, "fit_no_strat.RData"))

fit_no_strat_plots <- fit_no_strat %>% 
  ggcuminc(outcome = c("imminent", "death")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_no_strat_plots.pdf"),
    width = 10, height = 8)
print(fit_no_strat_plots, newpage = FALSE)
dev.off()


#stratify by the number of entries
cif_data_strat_entry <- data.frame()

for (i in (1:length(stratifiedCohort))){
  cif_data_strat_entry <- rbind(cif_data_strat_entry, 
                                stratifiedCohort[[i]] %>%
                                mutate(entry = i) %>%
                                mutate(follow_up_time = follow_up_time + (i-1)*730) %>%
                                select(subject_id, follow_up_time, entry) %>%
                                distinct() %>%
                                mutate(follow_up_time = as.integer(follow_up_time)/365.25))
}

cif_data_strat_entry <- cif_data_strat_entry %>% 
  inner_join(cif_data_no_strat, by = c("subject_id", "follow_up_time"))

fit_strat_by_entry <- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ entry, cif_data_strat_entry)
save(fit_strat_by_entry, file = here::here(output_folder, "fit_strat_by_entry.RData"))

fit_strat_by_entry_plots <- fit_strat_by_entry %>% 
  ggcuminc(outcome = c("imminent", "death")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_by_entry_plots.pdf"),
    width = 10, height = 8)
print(fit_strat_by_entry_plots, newpage = FALSE)
dev.off()

#stratify by sites
cif_data_strat_site <- data.frame()

for (i in (1:length(stratifiedCohort))){
  cif_data_strat_site <- rbind(cif_data_strat_site, 
                               stratifiedCohort[[i]] %>%
                               mutate(follow_up_time = follow_up_time + (i-1)*730) %>%
                               select(subject_id, condition_start_date, fracture_site, index_date, follow_up_end, follow_up_time) %>%
                               distinct() %>%
                               mutate(follow_up_time = as.integer(follow_up_time)/365.25))
}

imminent_id <- censor_by_imminent %>% pull(subject_id)

cif_data_strat_site2 <- 
  cif_data_strat_site %>%
  filter(subject_id %in% imminent_id) %>%
  filter(condition_start_date == index_date | condition_start_date == follow_up_end) %>%
  mutate(fracture_site = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                   fracture_site == "Hip" ~ "Hip",
                                   !(fracture_site %in% c("Vertebra", "Hip")) ~ "nhnv"
                                     ))

cif_data_strat_site_any_imm <- cif_data_strat_site2 %>%
  group_by(subject_id) %>%
  arrange(condition_start_date, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup() %>%
  rename(index_site = fracture_site) %>%
  select(subject_id, index_site, follow_up_time)

cif_data_strat_site_any_no_imm <- cif_data_strat_site %>%
  filter(!subject_id %in% imminent_id) %>%
  group_by(subject_id) %>%
  arrange(condition_start_date, .by_group = TRUE) %>% 
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(index_site = "None") %>%
  select(subject_id, index_site, follow_up_time)

cif_data_strat_site_any <- rbind(cif_data_strat_site_any_imm, cif_data_strat_site_any_no_imm)

cif_data_strat_site_any$index_site <- factor(cif_data_strat_site_any$index_site, levels = c("None", "Hip", "Vertebra", "nhnv"))

fit_strat_site_any <- tidycmprsk::cuminc(Surv(follow_up_time, index_site) ~ 1, cif_data_strat_site_any)
save(fit_strat_site_any, file = here::here(output_folder, "fit_strat_site_any.RData"))

fit_strat_site_any_plot <- fit_strat_site_any %>% 
  ggcuminc(outcome = c("Hip", "Vertebra", "nhnv")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_site_any_plot.pdf"),
    width = 10, height = 8)
print(fit_strat_site_any_plot, newpage = FALSE)
dev.off()

# vert 
vert_id <- cif_data_strat_site_any %>%
  filter (index_site == "Vertebra") %>%
  pull(subject_id)

cif_data_vert <- cif_data_strat_site2 %>% 
  filter(subject_id %in% vert_id) %>%
  group_by(subject_id) %>%
  arrange(condition_start_date, .by_group = TRUE) %>% 
  filter(row_number()==2) %>%
  ungroup()

cif_data_vert$fracture_site <- factor(cif_data_vert$fracture_site, levels = c("Vertebra", "Hip", "nhnv"))

fit_strat_site_vert <- tidycmprsk::cuminc(Surv(follow_up_time, fracture_site) ~ 1, cif_data_vert)
save(fit_strat_site_vert, file = here::here(output_folder, "fit_strat_site_vert.RData"))

fit_strat_site_vert_plot <- fit_strat_site_vert %>% 
  ggcuminc(outcome = c("Hip", "nhnv")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_site_vert_plot.pdf"),
    width = 10, height = 8)
print(fit_strat_site_vert_plot, newpage = FALSE)
dev.off()

# hip 
hip_id <- cif_data_strat_site_any %>%
  filter (index_site == "Hip") %>%
  pull(subject_id)

cif_data_hip <- cif_data_strat_site2 %>% 
  filter(subject_id %in% hip_id) %>%
  group_by(subject_id) %>%
  arrange(condition_start_date, .by_group = TRUE) %>% 
  filter(row_number()==2) %>%
  ungroup()

cif_data_hip$fracture_site <- factor(cif_data_hip$fracture_site, levels = c("Hip", "Vertebra", "nhnv"))

fit_strat_site_hip <- tidycmprsk::cuminc(Surv(follow_up_time, fracture_site) ~ 1, cif_data_hip)
save(fit_strat_site_hip, file = here::here(output_folder, "fit_strat_site_hip.RData"))

fit_strat_site_hip_plot <- fit_strat_site_hip %>% 
  ggcuminc(outcome = c("Vertebra", "nhnv")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_site_hip_plot.pdf"),
    width = 10, height = 8)
print(fit_strat_site_hip_plot, newpage = FALSE)
dev.off()

# nhnv
nhnv_id <- cif_data_strat_site_any %>%
  filter (index_site == "nhnv") %>%
  pull(subject_id)

cif_data_nhnv <- cif_data_strat_site2 %>% 
  filter(subject_id %in% nhnv_id) %>%
  group_by(subject_id) %>%
  arrange(condition_start_date, .by_group = TRUE) %>% 
  filter(row_number()==2) %>%
  ungroup()

cif_data_nhnv$fracture_site <- factor(cif_data_nhnv$fracture_site, levels = c("nhnv", "Vertebra", "Hip"))

fit_strat_site_nhnv <- tidycmprsk::cuminc(Surv(follow_up_time, fracture_site) ~ 1, cif_data_nhnv)
save(fit_strat_site_nhnv, file = here::here(output_folder, "fit_strat_site_nhnv.RData"))

fit_strat_site_nhnv_plot <- fit_strat_site_nhnv %>% 
  ggcuminc(outcome = c("Vertebra", "Hip")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_site_nhnv_plot.pdf"),
    width = 10, height = 8)
print(fit_strat_site_nhnv_plot, newpage = FALSE)
dev.off()
