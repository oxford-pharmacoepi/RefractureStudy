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

### Subgroups incidence
subgroupIncidenceResults <- list()
for (i in (1:length(entryTable))){
  subgroupIncidenceResults[[i]] <- tibble(fracture = sum(entryTable[[i]]$imminentFracture), py = as.integer(ObservedTime[[i]])/365.25, tibble(n_person = entryTable[[i]] %>% distinct(subject_id) %>% tally() %>% pull()))
  subgroupIncidenceResults[[i]] <- cbind(as.data.frame(PoissonCI(x=subgroupIncidenceResults[[i]]$fracture, n=subgroupIncidenceResults[[i]]$py/1000, method = "exact")) %>% mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% select(c(-lwr.ci, -upr.ci)), subgroupIncidenceResults[[i]], tibble(database_name=db_name), tibble(subgroup_id = i))

}

subgroupIncidenceResultsTable <- data.frame()
for (i in (1:length(subgroupIncidenceResults))){
  subgroupIncidenceResultsTable<-rbind(subgroupIncidenceResultsTable, subgroupIncidenceResults[[i]])
}

# obscuring according to min cell count
subgroupIncidenceResultsTable <- subgroupIncidenceResultsTable %>% 
  mutate(obscured = n_person < minimum_counts)

subgroupIncidenceResultsTable <- subgroupIncidenceResultsTable %>%
  mutate(est = ifelse(obscured == T, NA, est),
         lower = ifelse(obscured == T, NA, lower),
         upper = ifelse(obscured == T, NA, upper),
         fracture = ifelse(obscured == T, NA, fracture),
         py = ifelse(obscured == T, NA, py),
         n_person = ifelse(obscured == T, NA, n_person))

# Export Incidence Results
write.xlsx(inc_results, file = here::here(output_folder, "IncidenceResults.xlsx"))
write.xlsx(subgroupIncidenceResultsTable, file = here::here(output_folder, "SubgroupIncidenceResults.xlsx"))

rm(inc_results, 
   reverseEntryTable, 
   subgroupIncidenceResultsTable,
   AttritionReportRQ2,
   confidenceInterval,
   fracture_table_rq2,
   totalObservedTime,
   ObservedTime,
   fracture_table_back_up,
   fracture_table_follow_up,
   subgroupIncidenceResults)

### follow up time summary statistics
follow_up_time_stats <- data.frame()
stratifiedCohort <- list()
for (i in (1:(length(entryTable)-1))){
  stratifiedCohort[[i]] <- entryTable[[i]] %>% anti_join(entryTable[[i+1]], by = "subject_id")
}

stratifiedCohort[[length(entryTable)]] <- entryTable[[length(entryTable)]]

for (i in (1:length(stratifiedCohort))){
  follow_up_time_stats <- rbind(follow_up_time_stats, 
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

censor_by_imminent <- censor_by_imminent %>% pull(subject_id)

follow_up_time_stats <- follow_up_time_stats %>%
  mutate(group = ifelse(follow_up_time_stats$subject_id %in% censor_by_imminent, "imminent", "no imminent"))

fut_tot_percentiles <- quantile(follow_up_time_stats$follow_up_time, probs = c(.25, .5, .75))
fut_tot_percentiles_imm <- quantile(follow_up_time_stats %>% filter(group == "imminent") %>% pull(follow_up_time), probs = c(.25, .5, .75))
fut_tot_percentiles_no_imm <- quantile(follow_up_time_stats %>% filter(group == "no imminent") %>% pull(follow_up_time), probs = c(.25, .5, .75))

follow_up_time <- 
  data.frame(x = "Follow up time in years, median (IQR)",
           y = paste0(round(fut_tot_percentiles[2], 2), 
                      " (",
                      round(fut_tot_percentiles[1], 2),
                      "-",
                      round(fut_tot_percentiles[3], 2),
                      ")"),
           z = paste0(round(fut_tot_percentiles_imm[2], 2), 
                      " (",
                      round(fut_tot_percentiles_imm[1], 2),
                      "-",
                      round(fut_tot_percentiles_imm[3], 2),
                      ")"),
           w = paste0(round(fut_tot_percentiles_no_imm[2], 2), 
                      " (",
                      round(fut_tot_percentiles_no_imm[1], 2),
                      "-",
                      round(fut_tot_percentiles_no_imm[3], 2),
                      ")"))

updated_table_1 <- reformatted_table1
colnames(updated_table_1) <- c("x", "y", "z", "w")

updated_table_1 <- rbind(follow_up_time, updated_table_1)

write_csv(updated_table_1, here(output_folder, "updated_table_1.csv"))


### cumulative incidence function
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

fit_no_strat <- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ 1, cif_data_no_strat) %>%
  tbl_cuminc(times = c(1,2,3,4,5,6,7,8), 
             outcomes = c("imminent"),
             label_header = "**Year {time}**") %>%
  add_nevent() %>%
  add_n()

save(fit_no_strat, file = here::here(output_folder, "fit_no_strat.RData"))

fit_no_strat_plots_imminent <- fit_no_strat %>% 
  ggcuminc(outcome = c("imminent")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_no_strat_plots_imminent.pdf"),
    width = 10, height = 8)
print(fit_no_strat_plots_imminent, newpage = FALSE)
dev.off()

fit_no_strat_plots_death <- fit_no_strat %>% 
  ggcuminc(outcome = c("death")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_no_strat_plots_death.pdf"),
    width = 10, height = 8)
print(fit_no_strat_plots_death, newpage = FALSE)
dev.off()

#stratify by the number of entries
cif_data_strat_entry <- data.frame()

for (i in (1:length(stratifiedCohort))){
  if (stratifiedCohort[[i]] %>% distinct(subject_id) %>% tally() < 5) next 
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

cif_data_strat_site_index <- 
  cif_data_strat_site %>%
  filter(condition_start_date == index_date) %>%
  mutate(fracture_site = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                   fracture_site == "Hip" ~ "Hip",
                                   !(fracture_site %in% c("Vertebra", "Hip")) ~ "nhnv"
                                     )) %>%
  select(subject_id, fracture_site, condition_start_date) %>%
  inner_join(cif_data_no_strat, by = "subject_id")

unmasked_site <- cif_data_strat_site_index %>% group_by(fracture_site) %>% tally() %>% filter(n>=5) %>% pull(fracture_site)

cif_data_strat_site_index <- cif_data_strat_site_index %>% filter(fracture_site %in% unmasked_site)

fit_strat_index_site <- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ fracture_site, cif_data_strat_site_index)
save(fit_strat_index_site, file = here::here(output_folder, "fit_strat_index_site.RData"))

fit_strat_site_index_plot_death <- fit_strat_index_site %>% 
  ggcuminc(outcome = c("death")) +
  add_confidence_interval() 

pdf(here::here(plotFolder, "fit_strat_site_index_plot_death.pdf"),
    width = 10, height = 8)
print(fit_strat_site_index_plot_death, newpage = FALSE)
dev.off()

fit_strat_site_index_plot_imminent <- fit_strat_index_site %>% 
  ggcuminc(outcome = c("imminent")) +
  add_confidence_interval() 

pdf(here::here(plotFolder, "fit_strat_site_index_plot_imminent.pdf"),
    width = 10, height = 8)
print(fit_strat_site_index_plot_imminent, newpage = FALSE)
dev.off()

### breaking down imminent into different sites
imminent_cohort <- data.frame()
for (i in (1: length(entryTable))){
  imminent_cohort <- rbind(imminent_cohort, entryTable[[i]] %>% filter(imminentFracture==1))
}

imminent_cohort <- imminent_cohort %>% 
  select(subject_id, fracture_site) %>% 
  mutate(fracture_site = case_when(fracture_site == "Vertebra" ~ "Vertebra",
                                   fracture_site == "Hip" ~ "Hip",
                                   !(fracture_site %in% c("Vertebra", "Hip")) ~ "nhnv"
  )) %>%
  rename(imminent_site = fracture_site)

by_imminent_site <- cif_data_strat_site_index %>% 
  filter(status == "imminent") %>%
  select(-status) %>%
  inner_join(imminent_cohort, by = "subject_id") %>%
  rename(status = imminent_site) %>%
  mutate(status = paste("imminent fracture by", status, "fracture", sep = " "))

cif_data_strat_site_imminent <- rbind(cif_data_strat_site_index %>% 
                                        filter(!status == "imminent"), by_imminent_site)
rm(by_imminent_site, imminent_cohort)

cif_data_strat_site_imminent$status <- factor(cif_data_strat_site_imminent$status, levels = c("censor", "imminent fracture by Hip fracture", "imminent fracture by Vertebra fracture", "imminent fracture by nhnv fracture", "death"))

fit_strat_by_imminent_site <- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ fracture_site, cif_data_strat_site_imminent)
save(fit_strat_by_imminent_site, file = here::here(output_folder, "fit_strat_by_imminent_site.RData"))

fit_strat_by_imminent_site_plots2 <- fit_strat_by_imminent_site %>% 
  ggcuminc(outcome = c("death")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_by_imminent_site_plots_death.pdf"),
    width = 10, height = 8)
print(fit_strat_by_imminent_site_plots2, newpage = FALSE)
dev.off()

fit_strat_by_imminent_site_plots3 <- fit_strat_by_imminent_site %>% 
  ggcuminc(outcome = c("imminent fracture by nhnv fracture")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_by_imminent_site_plots_nhnv.pdf"),
    width = 10, height = 8)
print(fit_strat_by_imminent_site_plots3, newpage = FALSE)
dev.off()

fit_strat_by_imminent_site_plots4 <- fit_strat_by_imminent_site %>% 
  ggcuminc(outcome = c("imminent fracture by Vertebra fracture")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_by_imminent_site_plots_vert.pdf"),
    width = 10, height = 8)
print(fit_strat_by_imminent_site_plots4, newpage = FALSE)
dev.off()

fit_strat_by_imminent_site_plots5 <- fit_strat_by_imminent_site %>% 
  ggcuminc(outcome = c("imminent fracture by Hip fracture")) +
  add_confidence_interval()

pdf(here::here(plotFolder, "fit_strat_by_imminent_site_plots_hip.pdf"),
    width = 10, height = 8)
print(fit_strat_by_imminent_site_plots5, newpage = FALSE)
dev.off()
