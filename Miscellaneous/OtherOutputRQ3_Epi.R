load(here(sub_output_folder, "tempData","lasso_reg_01.RData"))
reg_output01 <- tibble()
for (i in (1:length(lasso_reg_01))){
  reg_output01 <- rbind(reg_output01, data.frame(lambda.1se = lasso_reg_01[[i]]$lambda.1se, period = i))
}
write.xlsx(reg_output01, file = here::here(sub_output_folder, "lasso_reg_output01.xlsx"))
   
load(here(sub_output_folder, "tempData","lasso_reg_12.RData"))
reg_output12 <- tibble()
for (i in (1:length(lasso_reg_12))){
  reg_output12 <- rbind(reg_output12, data.frame(lambda.1se = lasso_reg_12[[i]]$lambda.1se, period = i))
}
write.xlsx(reg_output12, file = here::here(sub_output_folder, "lasso_reg_output12.xlsx"))
rm(reg_output01, reg_output12, lasso_reg_01, lasso_reg_12)

##################### nn
load(here(sub_output_folder, "tempData","summary01.RData"))
summary_output_nn01 <- tibble()
summary01_back_up <- list()

for (i in (1:length(summary01))){
  summary01_back_up[[i]] <- summary01[[i]]$nn
  summary01_back_up[[i]] <- cbind(rownames(summary01_back_up[[i]]), data.frame(summary01_back_up[[i]], row.names=NULL))
  summary01_back_up[[i]] <- summary01_back_up[[i]] %>% dplyr::mutate(period = i)
}

for(i in (1:length(summary01))){
  summary_output_nn01 <- rbind(summary_output_nn01, summary01_back_up[[i]])
}

summary_output_nn01 <- summary_output_nn01 %>% 
  dplyr::rename(information_col = 'rownames(summary01_back_up[[i]])')
write.xlsx(summary_output_nn01, file = here::here(sub_output_folder, "summary_output_nn01.xlsx"))
rm(summary_output_nn01, summary01_back_up)

#########
load(here(sub_output_folder, "tempData","summary12.RData"))
summary_output_nn12 <- tibble()
summary12_back_up <- list()

for (i in (1:length(summary12))){
  summary12_back_up[[i]] <- summary12[[i]]$nn
  summary12_back_up[[i]] <- cbind(rownames(summary12_back_up[[i]]), data.frame(summary12_back_up[[i]], row.names=NULL))
  summary12_back_up[[i]] <- summary12_back_up[[i]] %>% dplyr::mutate(period = i)
}

for(i in (1:length(summary12))){
  summary_output_nn12 <- rbind(summary_output_nn12, summary12_back_up[[i]])
}

summary_output_nn12 <- summary_output_nn12 %>% 
  dplyr::rename(information_col = 'rownames(summary12_back_up[[i]])')
write.xlsx(summary_output_nn12, file = here::here(sub_output_folder, "summary_output_nn12.xlsx"))

##################### sum.all
load(here(sub_output_folder, "tempData","summary01.RData"))
summary_output_sum.all01 <- tibble()
summary01_back_up <- list()

for (i in (1:length(summary01))){
  summary01_back_up[[i]] <- summary01[[i]]$sum.all
  summary01_back_up[[i]] <- cbind(rownames(summary01_back_up[[i]]), data.frame(summary01_back_up[[i]], row.names=NULL))
  summary01_back_up[[i]] <- summary01_back_up[[i]] %>% dplyr::mutate(period = i)
}

for(i in (1:length(summary01))){
  summary_output_sum.all01 <- rbind(summary_output_sum.all01, summary01_back_up[[i]])
}

summary_output_sum.all01 <- summary_output_sum.all01 %>% 
  dplyr::rename(information_col = 'rownames(summary01_back_up[[i]])')
summary_output_sum.all01 <- summary_output_sum.all01 %>% 
  dplyr::filter(information_col!="subject_id")
write.xlsx(summary_output_sum.all01, file = here::here(sub_output_folder, "summary_output_sum.all01.xlsx"))
rm(summary_output_sum.all01, summary01_back_up)

#########
load(here(sub_output_folder, "tempData","summary12.RData"))
summary_output_sum.all12 <- tibble()
summary12_back_up <- list()

for (i in (1:length(summary12))){
  summary12_back_up[[i]] <- summary12[[i]]$sum.all
  summary12_back_up[[i]] <- cbind(rownames(summary12_back_up[[i]]), data.frame(summary12_back_up[[i]], row.names=NULL))
  summary12_back_up[[i]] <- summary12_back_up[[i]] %>% dplyr::mutate(period = i)
}

for(i in (1:length(summary12))){
  summary_output_sum.all12 <- rbind(summary_output_sum.all12, summary12_back_up[[i]])
}

summary_output_sum.all12 <- summary_output_sum.all12 %>% 
  dplyr::rename(information_col = 'rownames(summary12_back_up[[i]])')
summary_output_sum.all12 <- summary_output_sum.all12 %>% 
  dplyr::filter(information_col!="subject_id")
write.xlsx(summary_output_sum.all12, file = here::here(sub_output_folder, "summary_output_sum.all12.xlsx"))

##################### sum.matched
load(here(sub_output_folder, "tempData","summary01.RData"))
summary_output_sum.matched01 <- tibble()
summary01_back_up <- list()

for (i in (1:length(summary01))){
  summary01_back_up[[i]] <- summary01[[i]]$sum.matched
  summary01_back_up[[i]] <- cbind(rownames(summary01_back_up[[i]]), data.frame(summary01_back_up[[i]], row.names=NULL))
  summary01_back_up[[i]] <- summary01_back_up[[i]] %>% dplyr::mutate(period = i)
}

for(i in (1:length(summary01))){
  summary_output_sum.matched01 <- rbind(summary_output_sum.matched01, summary01_back_up[[i]])
}

summary_output_sum.matched01 <- summary_output_sum.matched01 %>% 
  dplyr::rename(information_col = 'rownames(summary01_back_up[[i]])')
summary_output_sum.matched01 <- summary_output_sum.matched01 %>% 
  dplyr::filter(information_col!="subject_id")
write.xlsx(summary_output_sum.matched01, file = here::here(sub_output_folder, "summary_output_sum.matched01.xlsx"))
rm(summary_output_sum.matched01, summary01_back_up)

#########
load(here(sub_output_folder, "tempData","summary12.RData"))
summary_output_sum.matched12 <- tibble()
summary12_back_up <- list()

for (i in (1:length(summary12))){
  summary12_back_up[[i]] <- summary12[[i]]$sum.matched
  summary12_back_up[[i]] <- cbind(rownames(summary12_back_up[[i]]), data.frame(summary12_back_up[[i]], row.names=NULL))
  summary12_back_up[[i]] <- summary12_back_up[[i]] %>% dplyr::mutate(period = i)
}

for(i in (1:length(summary12))){
  summary_output_sum.matched12 <- rbind(summary_output_sum.matched12, summary12_back_up[[i]])
}

summary_output_sum.matched12 <- summary_output_sum.matched12 %>% 
  dplyr::rename(information_col = 'rownames(summary12_back_up[[i]])')
summary_output_sum.matched12 <- summary_output_sum.matched12 %>% 
  dplyr::filter(information_col!="subject_id")
write.xlsx(summary_output_sum.matched12, file = here::here(sub_output_folder, "summary_output_sum.matched12.xlsx"))
