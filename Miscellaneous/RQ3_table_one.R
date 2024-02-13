stem_table <- "rq3"
conditions <- paste0(stem_table, "_conditions")
medications <- paste0(stem_table, "_medications")

########################## LOADING NECESSARY DATA #############################
load(here::here(sub_output_folder, "tempData", "subclasses01.RData"))
load(here::here(sub_output_folder, "tempData", "subclasses12.RData"))
load(here::here(sub_output_folder, "tempData", "targetCohort.RData"))
load(here::here(sub_output_folder, "tempData", "compCohort1.RData"))
load(here::here(sub_output_folder, "tempData", "compCohort2.RData"))

t1_sub_output_folder <- here(sub_output_folder, "tableones_rq3")
if (!dir.exists(t1_sub_output_folder)) {
  dir.create(t1_sub_output_folder)
}

tot_periods_target <- length(targetCohort)
tot_periods_c1 <- length(compCohort1)

print(paste0("Running table 1 before matching at ", Sys.time()))
source(here("Miscellaneous", "before_matching.R"))

print(paste0("Running table 1 after matching 01 at ", Sys.time()))
source(here("Miscellaneous", "after_matching01.R"))

print(paste0("Running table 1 after matching 12 at ", Sys.time()))
source(here("Miscellaneous", "after_matching12.R"))

print(paste0("Running table 1 across periods at ", Sys.time()))
source(here("Miscellaneous", "across_periods.R"))
