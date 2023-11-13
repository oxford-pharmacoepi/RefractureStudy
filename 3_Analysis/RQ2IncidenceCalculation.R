plotFolder <- here(sub_output_folder, "plots")
if (!dir.exists(plotFolder)) {
  dir.create(plotFolder)
}
options("ggsurvfit.switch-color-linetype" = T)

info(logger, "CALCULATING OVERALL INCIDENCE AND BY SUBGROUPS")
ObservedTime <- list()

for (i in (1:length(entryTable))){
  ObservedTime[[i]] <- entryTable[[i]] %>% 
    dplyr::select(subject_id, follow_up_time) %>% 
    dplyr::distinct() %>% 
    dplyr::summarise(total_time = sum(follow_up_time))
}

totalObservedTime <- 0
for (i in (1:length(ObservedTime))){
  totalObservedTime <- totalObservedTime + ObservedTime[[i]]
}

totalObservedTimeYears <- as.integer(totalObservedTime)/365.25

totalFracture <- 0
for (i in (1:length(entryTable))){
  totalFracture <- totalFracture + sum(entryTable[[i]]$imminentFracture)
}

inc_results <- tibble(fracture = totalFracture, 
                      py = totalObservedTimeYears)

confidenceInterval <- PoissonCI(x=totalFracture, n=totalObservedTimeYears/1000, method = "exact")
confidenceInterval <- as.data.frame(confidenceInterval) %>% dplyr::mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% dplyr::select(c(-lwr.ci, -upr.ci))
inc_results <- cbind(inc_results, tibble(n_person = entryTable[[1]] %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull()), confidenceInterval, tibble(database_name=db_name)) 

### Subgroups incidence
subgroupIncidenceResults <- list()
for (i in (1:length(entryTable))){
  subgroupIncidenceResults[[i]] <- tibble(fracture = sum(entryTable[[i]]$imminentFracture), py = as.integer(ObservedTime[[i]])/365.25, tibble(n_person = entryTable[[i]] %>% dplyr::distinct(subject_id) %>% dplyr::tally() %>% dplyr::pull()))
  subgroupIncidenceResults[[i]] <- cbind(as.data.frame(PoissonCI(x=subgroupIncidenceResults[[i]]$fracture, n=subgroupIncidenceResults[[i]]$py/1000, method = "exact")) %>% dplyr::mutate(est = round(est,2), lower = round(lwr.ci, 2), upper = round(upr.ci,2)) %>% dplyr::select(c(-lwr.ci, -upr.ci)), subgroupIncidenceResults[[i]], tibble(database_name=db_name), tibble(subgroup_id = i))

}

subgroupIncidenceResultsTable <- data.frame()
for (i in (1:length(subgroupIncidenceResults))){
  subgroupIncidenceResultsTable<-rbind(subgroupIncidenceResultsTable, subgroupIncidenceResults[[i]])
}

# obscuring according to min cell count
subgroupIncidenceResultsTable <- subgroupIncidenceResultsTable %>% 
  dplyr::mutate(obscured = n_person < minimum_counts)

subgroupIncidenceResultsTable <- subgroupIncidenceResultsTable %>%
  dplyr::mutate(est = ifelse(obscured == T, NA, est),
         lower = ifelse(obscured == T, NA, lower),
         upper = ifelse(obscured == T, NA, upper),
         fracture = ifelse(obscured == T, NA, fracture),
         py = ifelse(obscured == T, NA, py),
         n_person = ifelse(obscured == T, NA, n_person))

# Export Incidence Results
write.xlsx(inc_results, file = here::here(sub_output_folder, "IncidenceResults.xlsx"))
write.xlsx(subgroupIncidenceResultsTable, file = here::here(sub_output_folder, "SubgroupIncidenceResults.xlsx"))
info(logger, "CALCULATING OVERALL INCIDENCE AND BY SUBGROUPS IS DONE")

### follow up time summary statistics
info(logger, "UPDATING TABLE 1 TO INCLUDE FOLLOW UP TIME")
follow_up_time_stats <- data.frame()
cohortEntry <- list()
for (i in (1:(length(entryTable)-1))){
  cohortEntry[[i]] <- entryTable[[i]] %>% dplyr::anti_join(entryTable[[i+1]], by = "subject_id")
}

cohortEntry[[length(entryTable)]] <- entryTable[[length(entryTable)]]

for (i in (1:length(cohortEntry))){
  follow_up_time_stats <- rbind(follow_up_time_stats, 
                                cohortEntry[[i]] %>%
                               dplyr::mutate(follow_up_time = follow_up_time + (i-1)*730) %>%
                               dplyr::select(subject_id, follow_up_time) %>%
                               dplyr::distinct() %>%
                               dplyr::mutate(follow_up_time = as.integer(follow_up_time)/365.25))
}

censor_by_imminent <- data.frame()
for (i in (1: length(entryTable))){
 censor_by_imminent <- rbind(censor_by_imminent, 
                             entryTable[[i]] %>% dplyr::filter(imminentFracture==1) %>% dplyr::select(subject_id))
}

censor_by_imminent <- censor_by_imminent %>% dplyr::pull(subject_id)

follow_up_time_stats <- follow_up_time_stats %>%
  dplyr::mutate(group = ifelse(follow_up_time_stats$subject_id %in% censor_by_imminent, "imminent", "no imminent"))

fut_tot_percentiles <- quantile(follow_up_time_stats$follow_up_time, probs = c(.25, .5, .75))
fut_tot_percentiles_imm <- quantile(follow_up_time_stats %>% dplyr::filter(group == "imminent") %>% dplyr::pull(follow_up_time), probs = c(.25, .5, .75))
fut_tot_percentiles_no_imm <- quantile(follow_up_time_stats %>% dplyr::filter(group == "no imminent") %>% dplyr::pull(follow_up_time), probs = c(.25, .5, .75))

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

updated_table_1 <- reformatted_table_1
colnames(updated_table_1) <- c("x", "y", "z", "w")

updated_table_1 <- rbind(follow_up_time, updated_table_1)
colnames(updated_table_1)<-colnames(reformatted_table_1)

write_csv(updated_table_1, here(sub_output_folder, "updated_table_1.csv"))
info(logger, "UPDATING TABLE 1 TO INCLUDE FOLLOW UP TIME IS DONE")

### CIF - Subgroup Analysis
info(logger, "CALCULATING CUMULATIVE INCIDENCE FUNCTION")
censor_by_imminent <- list()
censor_by_death <- list()
index_fracture <- list()

for (i in (1:length(entryTable))){
  censor_by_imminent[[i]] <- entryTable[[i]] %>% dplyr::filter(imminentFracture==1)
}

for (i in (1:length(entryTable))){
  censor_by_death[[i]] <- entryTable[[i]] %>% dplyr::filter(follow_up_end==death_date) %>%
    dplyr::anti_join(censor_by_imminent[[i]], by = "subject_id")
}

for (i in (1:length(entryTable))){
  index_fracture[[i]] <- entryTable[[i]] %>% dplyr::filter(condition_start_date==index_date)
}

####### cumulative incidence function
### making subgroup data
data_cif <- list()
for (i in (1:length(entryTable))){
  data_cif[[i]] <- index_fracture[[i]] %>% 
    dplyr::select(subject_id, follow_up_time, fracture_site) %>%
    dplyr::mutate(follow_up_time = as.numeric(follow_up_time)) %>%
    dplyr::rename(index_site = fracture_site) %>%
    dplyr::mutate(index_site = case_when(index_site == "Vertebra" ~ "Vertebra",
                                  index_site == "Hip" ~ "Hip",
                                  !(index_site %in% c("Vertebra", "Hip")) ~ "Non-hip, non-vertebra"
    ))
  censor_by_death_ids <- censor_by_death[[i]] %>% dplyr::select(subject_id) %>% dplyr::pull(subject_id)  
  censor_by_imminent_ids <- censor_by_imminent[[i]] %>% dplyr::select(subject_id) %>% dplyr::pull(subject_id)
  censor_by_imminent_hip_ids <- censor_by_imminent[[i]] %>% dplyr::filter(fracture_site == "Hip") %>% dplyr::select(subject_id) %>% dplyr::pull(subject_id)
  censor_by_imminent_vert_ids <- censor_by_imminent[[i]] %>% dplyr::filter(fracture_site == "Vertebra") %>% dplyr::select(subject_id) %>% dplyr::pull(subject_id)
  censor_by_imminent_nhnv_ids <- censor_by_imminent[[i]] %>% dplyr::filter(!(fracture_site %in% c("Hip", "Vertebra"))) %>% dplyr::select(subject_id) %>% dplyr::pull(subject_id)
  
  data_cif[[i]] <- data_cif[[i]] %>% dplyr::mutate(status = case_when((subject_id %in% censor_by_death_ids) ~ "death",
                                              (subject_id %in% censor_by_imminent_ids) ~ "imminent",
                                              !(subject_id %in% c(censor_by_death_ids, censor_by_imminent_ids)) ~ "censor")) %>%
    dplyr::mutate(subgroup_status = case_when((subject_id %in% censor_by_death_ids) ~ "death",
                                       (subject_id %in% censor_by_imminent_hip_ids) ~ "hip fracture",
                                       (subject_id %in% censor_by_imminent_vert_ids) ~ "vertebra fracture",
                                       (subject_id %in% censor_by_imminent_nhnv_ids) ~ "non-hip, non-vertebra fracture",
                                       !(subject_id %in% c(censor_by_death_ids, censor_by_imminent_hip_ids, censor_by_imminent_vert_ids, censor_by_imminent_nhnv_ids)) ~ "censor"))
  
}

### 1. plot 1s
counts_overall <- data.frame()
for (i in (1:length(data_cif))){
  counts_overall <- rbind(counts_overall,
  tibble(subgroup = i, 
         n = as.integer(data_cif[[i]] %>% tally()),
         n_imminent = as.integer(data_cif[[i]] %>% dplyr::filter(status == "imminent") %>% tally()),
         n_imminent_hip = as.integer(data_cif[[i]] %>% dplyr::filter(subgroup_status == "hip fracture") %>% tally()),
         n_imminent_vert = as.integer(data_cif[[i]] %>% dplyr::filter(subgroup_status == "vertebra fracture") %>% tally()),
         n_imminent_nhnv = as.integer(data_cif[[i]] %>% dplyr::filter(subgroup_status == "non-hip, non-vertebra fracture") %>% tally()))
  )
}
counts_overall <- counts_overall %>%
  dplyr::mutate(counts = ifelse((n<minimum_counts & n>0), paste0("<", minimum_counts), n),
                counts_imminent = ifelse((n_imminent<minimum_counts & n_imminent>0), paste0("<", minimum_counts), n_imminent),
                counts_imminent_hip = ifelse((n_imminent_hip<minimum_counts & n_imminent_hip>0), paste0("<", minimum_counts), n_imminent_hip),
                counts_imminent_vert = ifelse((n_imminent_vert<minimum_counts & n_imminent_vert>0), paste0("<", minimum_counts), n_imminent_vert),
                counts_imminent_nhnv = ifelse((n_imminent_nhnv<minimum_counts & n_imminent_nhnv>0), paste0("<", minimum_counts), n_imminent_nhnv)) %>%
  dplyr::select(-n, -n_imminent, -n_imminent_hip, -n_imminent_vert, -n_imminent_nhnv)
write.xlsx(counts_overall, file = here(sub_output_folder, "counts_overall.xlsx"))

first_plots_data <- data_cif[sapply(data_cif, nrow) >= minimum_counts]
first_plots <- list()
first_plots_table <- list()

first_plots_data[[1]]$status <- factor(first_plots_data[[1]]$status, levels = c("censor", "imminent", "death"))

first_plots_table[[1]]<- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ 1, first_plots_data[[1]]) %>%
  tbl_cuminc(times = c(0, 91, 183, 274, 365, 456, 548, 639, 730), 
             outcomes = c("imminent"),
             label_header = "**Day {time}**",
             label = "Subgroup 1, overall")

first_plots[[1]]<-tidycmprsk::cuminc(Surv(follow_up_time, status) ~ 1, first_plots_data[[1]]) %>%
  ggcuminc(outcome = c("imminent", "death")) +
  add_confidence_interval() +
  theme(plot.title = element_text(hjust = 0.5))

for (i in (2:length(first_plots_data))){
  if(first_plots_data[[i]] %>% filter(status=="imminent") %>% tally()==0) next
  first_plots_data[[i]]$status <- factor(first_plots_data[[i]]$status, levels = c("censor", "imminent", "death"))
  
  first_plots_table[[i]]<- tidycmprsk::cuminc(Surv(follow_up_time, status) ~ 1, first_plots_data[[i]]) %>%
    tbl_cuminc(times = c(0, 91, 183, 274, 365, 456, 548, 639, 730), 
               outcomes = c("imminent"),
               label_header = "**Day {time}**",
               label = paste0("Subgroup ", i, ", overall"))
  
  first_plots[[i]]<-tidycmprsk::cuminc(Surv(follow_up_time, status) ~ 1, first_plots_data[[i]]) %>%
    ggcuminc(outcome = c("imminent")) +
    add_confidence_interval() +
    xlab("Time (days)") + ylab("Cumulative Incidence of Imminent Fracture")
    theme(plot.title = element_text(hjust = 0.5))
}
rm(first_plots_data)

### 2. plot 2s
hip_plots_data <- list()
hip_plots <- list()
hip_plots_table <- list()

for (i in (1:length(data_cif))){
  hip_plots_data[[i]] <- data_cif[[i]] %>% dplyr::filter(index_site == "Hip")
}
hip_plots_data <- hip_plots_data[sapply(hip_plots_data, nrow) >= 1]

counts_hip <- data.frame()
for (i in (1:length(hip_plots_data))){
  counts_hip <- rbind(counts_hip,
                          tibble(subgroup = i, 
                                 n = as.integer(hip_plots_data[[i]] %>% dplyr::tally()),
                                 n_imminent = as.integer(hip_plots_data[[i]] %>% dplyr::filter(status == "imminent") %>% dplyr::tally()),
                                 n_imminent_hip = as.integer(hip_plots_data[[i]] %>% dplyr::filter(subgroup_status == "hip fracture") %>% tally()),
                                 n_imminent_vert = as.integer(hip_plots_data[[i]] %>% dplyr::filter(subgroup_status == "vertebra fracture") %>% tally()),
                                 n_imminent_nhnv = as.integer(hip_plots_data[[i]] %>% dplyr::filter(subgroup_status == "non-hip, non-vertebra fracture") %>% tally())))
}
counts_hip <- counts_hip %>%
  dplyr::mutate(counts = ifelse((n<minimum_counts & n>0), paste0("<", minimum_counts), n),
                counts_imminent = ifelse((n_imminent<minimum_counts & n_imminent>0), paste0("<", minimum_counts), n_imminent),
                counts_imminent_hip = ifelse((n_imminent_hip<minimum_counts & n_imminent_hip>0), paste0("<", minimum_counts), n_imminent_hip),
                counts_imminent_vert = ifelse((n_imminent_vert<minimum_counts & n_imminent_vert>0), paste0("<", minimum_counts), n_imminent_vert),
                counts_imminent_nhnv = ifelse((n_imminent_nhnv<minimum_counts & n_imminent_nhnv>0), paste0("<", minimum_counts), n_imminent_nhnv)) %>%
  dplyr::select(-n, -n_imminent, -n_imminent_hip, -n_imminent_vert, -n_imminent_nhnv)
write.xlsx(counts_hip, file = here(sub_output_folder, "counts_hip.xlsx"))

hip_plots_data <- hip_plots_data[sapply(hip_plots_data, nrow) >= minimum_counts]

for (i in (1:length(hip_plots_data))){
  if(hip_plots_data[[i]] %>% filter(status=="imminent") %>% tally()==0) next
  level <- hip_plots_data[[i]] %>% 
    dplyr::filter(!subgroup_status=="censor") %>%
    dplyr::mutate(subgroup_status = as.character(subgroup_status)) %>%
    dplyr::pull(subgroup_status) %>%
    unique()
  level <- c("censor", level)
  hip_plots_data[[i]]$subgroup_status <- factor(hip_plots_data[[i]]$subgroup_status, levels = level)
  
  hip_plots_table[[i]]<- tidycmprsk::cuminc(Surv(follow_up_time, subgroup_status) ~ 1, hip_plots_data[[i]]) %>%
    tbl_cuminc(times = c(0, 91, 183, 274, 365, 456, 548, 639, 730), 
               outcomes = setdiff(hip_plots_data[[i]] %>% select(subgroup_status) %>% distinct() %>% pull(), c("death", "censor")),
               label_header = "**Day {time}**", 
               label = paste0("index fracture: Hip, subgroup ", i)) 
  
  hip_plots[[i]]<-tidycmprsk::cuminc(Surv(follow_up_time, subgroup_status) ~ 1, hip_plots_data[[i]]) %>%
    ggcuminc(setdiff(hip_plots_data[[i]] %>% select(subgroup_status) %>% distinct() %>% pull(), c("death", "censor"))) +
    add_confidence_interval() +
    xlab("Time (days)") + ylab("Cumulative Incidence of Imminent Fracture") +
    theme(plot.title = element_text(hjust = 0.5))
}
rm(hip_plots_data)

### 3. plot 3s
vert_plots_data <- list()
vert_plots <- list()
vert_plots_table <- list()

for (i in (1:length(data_cif))){
  vert_plots_data[[i]] <- data_cif[[i]] %>% dplyr::filter(index_site == "Vertebra")
}
vert_plots_data <- vert_plots_data[sapply(vert_plots_data, nrow) >= 1]

counts_vert <- data.frame()
for (i in (1:length(vert_plots_data))){
  counts_vert <- rbind(counts_vert,
                      tibble(subgroup = i, 
                             n = as.integer(vert_plots_data[[i]] %>% dplyr::tally()),
                             n_imminent = as.integer(vert_plots_data[[i]] %>% dplyr::filter(status == "imminent") %>% dplyr::tally()),
                             n_imminent_hip = as.integer(vert_plots_data[[i]] %>% dplyr::filter(subgroup_status == "hip fracture") %>% tally()),
                             n_imminent_vert = as.integer(vert_plots_data[[i]] %>% dplyr::filter(subgroup_status == "vertebra fracture") %>% tally()),
                             n_imminent_nhnv = as.integer(vert_plots_data[[i]] %>% dplyr::filter(subgroup_status == "non-hip, non-vertebra fracture") %>% tally())))
}
counts_vert <- counts_vert %>%
  dplyr::mutate(counts = ifelse((n<minimum_counts & n>0), paste0("<", minimum_counts), n),
                counts_imminent = ifelse((n_imminent<minimum_counts & n_imminent>0), paste0("<", minimum_counts), n_imminent),
                counts_imminent_hip = ifelse((n_imminent_hip<minimum_counts & n_imminent_hip>0), paste0("<", minimum_counts), n_imminent_hip),
                counts_imminent_vert = ifelse((n_imminent_vert<minimum_counts & n_imminent_vert>0), paste0("<", minimum_counts), n_imminent_vert),
                counts_imminent_nhnv = ifelse((n_imminent_nhnv<minimum_counts & n_imminent_nhnv>0), paste0("<", minimum_counts), n_imminent_nhnv)) %>%
  dplyr::select(-n, -n_imminent, -n_imminent_hip, -n_imminent_vert, -n_imminent_nhnv)
write.xlsx(counts_vert, file = here(sub_output_folder, "counts_vert.xlsx"))

vert_plots_data <- vert_plots_data[sapply(vert_plots_data, nrow) >= minimum_counts]

for (i in (1:length(vert_plots_data))){
  if(vert_plots_data[[i]] %>% filter(status=="imminent") %>% tally()==0) next
  level <- vert_plots_data[[i]] %>% 
    dplyr::filter(!subgroup_status=="censor") %>%
    dplyr::mutate(subgroup_status = as.character(subgroup_status)) %>%
    dplyr::pull(subgroup_status) %>%
    unique()
  level <- c("censor", level)
  vert_plots_data[[i]]$subgroup_status <- factor(vert_plots_data[[i]]$subgroup_status, levels = level)
  
  vert_plots_table[[i]]<- tidycmprsk::cuminc(Surv(follow_up_time, subgroup_status) ~ 1, vert_plots_data[[i]]) %>%
    tbl_cuminc(times = c(0, 91, 183, 274, 365, 456, 548, 639, 730), 
               outcomes = setdiff(vert_plots_data[[i]] %>% select(subgroup_status) %>% distinct() %>% pull(), c("death", "censor")),
               label_header = "**Day {time}**",
               label = paste0("index fracture: Vertebra, subgroup ", i)) 
  
  vert_plots[[i]]<-tidycmprsk::cuminc(Surv(follow_up_time, subgroup_status) ~ 1, vert_plots_data[[i]]) %>%
    ggcuminc(outcome = setdiff(vert_plots_data[[i]] %>% select(subgroup_status) %>% distinct() %>% pull(), c("death", "censor"))) +
    add_confidence_interval() +
    xlab("Time (days)") + ylab("Cumulative Incidence of Imminent Fracture")
    theme(plot.title = element_text(hjust = 0.5))
}
rm(vert_plots_data)

### 4. plot 4s
nhnv_plots_data <- list()
nhnv_plots <- list()
nhnv_plots_table <- list()

for (i in (1:length(data_cif))){
  nhnv_plots_data[[i]] <- data_cif[[i]] %>% dplyr::filter(index_site == "Non-hip, non-vertebra")
}
nhnv_plots_data <- nhnv_plots_data[sapply(nhnv_plots_data, nrow) >= 1]

counts_nhnv <- data.frame()
for (i in (1:length(nhnv_plots_data))){
  counts_nhnv <- rbind(counts_nhnv,
                       tibble(subgroup = i, 
                              n = as.integer(nhnv_plots_data[[i]] %>% dplyr::tally()),
                              n_imminent = as.integer(nhnv_plots_data[[i]] %>% dplyr::filter(status == "imminent") %>% dplyr::tally()),
                              n_imminent_hip = as.integer(nhnv_plots_data[[i]] %>% dplyr::filter(subgroup_status == "hip fracture") %>% tally()),
                              n_imminent_vert = as.integer(nhnv_plots_data[[i]] %>% dplyr::filter(subgroup_status == "vertebra fracture") %>% tally()),
                              n_imminent_nhnv = as.integer(nhnv_plots_data[[i]] %>% dplyr::filter(subgroup_status == "non-hip, non-vertebra fracture") %>% tally())))
}
counts_nhnv <- counts_nhnv %>%
  dplyr::mutate(counts = ifelse((n<minimum_counts & n>0), paste0("<", minimum_counts), n),
                counts_imminent = ifelse((n_imminent<minimum_counts & n_imminent>0), paste0("<", minimum_counts), n_imminent),
                counts_imminent_hip = ifelse((n_imminent_hip<minimum_counts & n_imminent_hip>0), paste0("<", minimum_counts), n_imminent_hip),
                counts_imminent_vert = ifelse((n_imminent_vert<minimum_counts & n_imminent_vert>0), paste0("<", minimum_counts), n_imminent_vert),
                counts_imminent_nhnv = ifelse((n_imminent_nhnv<minimum_counts & n_imminent_nhnv>0), paste0("<", minimum_counts), n_imminent_nhnv)) %>%
  dplyr::select(-n, -n_imminent, -n_imminent_hip, -n_imminent_vert, -n_imminent_nhnv)
write.xlsx(counts_nhnv, file = here(sub_output_folder, "counts_nhnv.xlsx"))

nhnv_plots_data <- nhnv_plots_data[sapply(nhnv_plots_data, nrow) >= minimum_counts]

for (i in (1:length(nhnv_plots_data))){
  if(nhnv_plots_data[[i]] %>% filter(status=="imminent") %>% tally()==0) next
  level <- nhnv_plots_data[[i]] %>% 
    dplyr::filter(!subgroup_status=="censor") %>%
    dplyr::mutate(subgroup_status = as.character(subgroup_status)) %>%
    dplyr::pull(subgroup_status) %>%
    unique()
  level <- c("censor", level)
  nhnv_plots_data[[i]]$subgroup_status <- factor(nhnv_plots_data[[i]]$subgroup_status, levels = level)
  
  nhnv_plots_table[[i]]<- tidycmprsk::cuminc(Surv(follow_up_time, subgroup_status) ~ 1, nhnv_plots_data[[i]]) %>%
    tbl_cuminc(times = c(0, 91, 183, 274, 365, 456, 548, 639, 730), 
               outcomes = setdiff(nhnv_plots_data[[i]] %>% select(subgroup_status) %>% distinct() %>% pull(), c("death", "censor")),
               label_header = "**Day {time}**",
               label = paste0("index fracture: Non-hip, non-vertebra, subgroup ", i)) 
  
  nhnv_plots[[i]]<-tidycmprsk::cuminc(Surv(follow_up_time, subgroup_status) ~ 1, nhnv_plots_data[[i]]) %>%
    ggcuminc(outcome = setdiff(nhnv_plots_data[[i]] %>% select(subgroup_status) %>% distinct() %>% pull(), c("death", "censor"))) +
    add_confidence_interval() +
    xlab("Time (days)") + ylab("Cumulative Incidence of Imminent Fracture")
    theme(plot.title = element_text(hjust = 0.5))
}
rm(nhnv_plots_data)

### export results
for (i in (1:length(first_plots))){
  name <- paste0("CIF_subgroup", i, ".png")
  ggsave(file = file.path(here(plotFolder, name)), 
         plot = first_plots[[i]],
         width = 15,
         height = 8)
}

for (i in (1:length(hip_plots))){
  name <- paste0("CIF_hip_subgroup", i, ".png")
  ggsave(file = file.path(here(plotFolder, name)), 
         plot = hip_plots[[i]],
         width = 15,
         height = 8)
}

for (i in (1:length(vert_plots))){
  name <- paste0("CIF_vert_subgroup", i, ".png")
  ggsave(file = file.path(here(plotFolder, name)), 
         plot = vert_plots[[i]],
         width = 15,
         height = 8)
}

for (i in (1:length(nhnv_plots))){
  name <- paste0("CIF_nhnv_subgroup", i, ".png")
  ggsave(file = file.path(here(plotFolder, name)), 
         plot = nhnv_plots[[i]],
         width = 15,
         height = 8)
}

for (i in (1:length(first_plots_table))){
  name <- paste0("CIF_subgroup", i, ".docx")
  first_plots_table[[i]] %>%
    gtsummary::as_gt() %>%
    gt::gtsave(filename = here(plotFolder, name))
}

for (i in (1:length(hip_plots_table))){
  name <- paste0("CIF_hip_subgroup", i, ".docx")
  hip_plots_table[[i]] %>%
    gtsummary::as_gt() %>%
    gt::gtsave(filename = here(plotFolder, name))
}

for (i in (1:length(vert_plots_table))){
  name <- paste0("CIF_vert_subgroup", i, ".docx")
  vert_plots_table[[i]] %>%
    gtsummary::as_gt() %>%
    gt::gtsave(filename = here(plotFolder, name))
}

for (i in (1:length(nhnv_plots_table))){
  name <- paste0("CIF_nhnv_subgroup", i, ".docx")
  nhnv_plots_table[[i]] %>%
    gtsummary::as_gt() %>%
    gt::gtsave(filename = here(plotFolder, name))
}

info(logger, "CALCULATING CUMULATIVE INCIDENCE FUNCTION IS DONE")
rm(inc_results, 
   subgroupIncidenceResultsTable,
   AttritionReportRQ2,
   confidenceInterval,
   totalObservedTime,
   ObservedTime,
   fracture_table_back_up,
   subgroupIncidenceResults,
   counts_hip,
   counts_nhnv,
   counts_overall,
   counts_vert,
   first_plots,
   first_plots_table,
   vert_plots,
   vert_plots_table,
   hip_plots,
   hip_plots_table,
   nhnv_plots,
   nhnv_plots_table,
   censor_by_death,
   censor_by_imminent,
   censor_by_death_ids,
   censor_by_imminent_hip_ids,
   censor_by_imminent_ids,
   censor_by_imminent_nhnv_ids,
   censor_by_imminent_vert_ids,
   data_cif,
   follow_up_time,
   follow_up_time_stats,
   reformatted_table_1,
   fracture_table_rq2)
