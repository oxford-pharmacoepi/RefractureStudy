# for a fracture table, compute the index date for each person
addIndex <- function (fractureTable){
  fractureTable %>%
    right_join(fractureTable %>% 
                 filter (condition_start_date >= cohort_start_date) %>%
                 filter (condition_start_date <= cohort_end_date) %>% 
                 group_by (subject_id) %>% 
                 summarise (index_date = min(condition_start_date, na.rm = T)), by = "subject_id")
}

# for a fracture table, removing individuals with index fracture on the same day as a record of death
noDeathOnIndex <- function (fractureTable){
  fractureTable %>% anti_join(cdm[["death"]], by = c("subject_id" = "person_id", "index_date" = "death_date"), copy = T)
}

noDeathOnOrAfterIndex <- function(fractureTable){
  fractureTable %>%
    left_join(cdm[["death"]], by = c("subject_id"= "person_id"), copy = T) %>% 
    filter(death_date >= index_date |is.na(death_date)) %>%
    select(colnames(fractureTable))
}

# for a fracture table, removing individuals with cancer before or on the same day as the index fracture
noCancerPriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                select(-cohort_start_date, -cohort_end_date) %>%
                inner_join(cdm[["cancer"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cancer_date<=index_date), by = "subject_id")
}

# for a fracture table, removing individuals with bone disease before or on the same day as the index fracture
noBoneDiseasePriorOrOnIndex <- function (fractureTable){
  fractureTable %>% 
    anti_join(fractureTable %>% 
                select(-cohort_start_date, -cohort_end_date) %>%
                inner_join(cdm[["mbd"]], by = "subject_id", copy = T, relationship = "many-to-many") %>%
                filter(cohort_start_date<=index_date), by = "subject_id")
}

# add an extra variable after_index which is two years after the index date
addInTwoYearsAfter <- function (fractureTable){
  fractureTable %>%
    mutate(after_index = index_date + 730)
}

# add in the end of observation period for each individual
addInObsEndDate <- function (fractureTable){
  fractureTable %>% 
    left_join(cdm[["observation_period"]], by = c("subject_id" = "person_id"), copy = T) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, after_index, observation_period_end_date)
}

# add in a column indicating the date of cancer after the index date
addInCancerPostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[["cancer"]] %>% rename(cancer_concept_id = condition_concept_id), 
                                           by = c("subject_id", "cohort_start_date", "cohort_end_date"), 
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < cancer_date) %>%   
                                group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(cancer_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date")) %>%
    select(-cancer_concept_id) 
}

# add in a column indicating the date of bone disease after the index date (has to be done after the cancer one)
addInBoneDiseasePostIndex <- function (fractureTable){
  fractureTable %>% left_join(fractureTable %>% 
                                inner_join(cdm[["mbd"]] %>%
                                             rename(bone_disease_concept_id = condition_concept_id), 
                                           by = c("subject_id", "cohort_start_date", "cohort_end_date"),
                                           copy = T, 
                                           relationship = "many-to-many") %>%
                                filter(index_date < mbd_date) %>%   
                                group_by(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, observation_period_end_date) %>%
                                arrange(mbd_date) %>%
                                filter(row_number()==1) %>%
                                ungroup(), by = c("subject_id", "cohort_start_date", "cohort_end_date", "condition_concept_id", "condition_start_date", "fracture_site", "index_date", "after_index", "observation_period_end_date", "cancer_date")) %>%
    select(-bone_disease_concept_id) 
}

# add a column of next fracture after the index one
addInNextFracture <- function(fractureTable){
  fractureTable %>% 
    left_join(fractureTable %>% 
                filter(condition_start_date> index_date) %>% 
                group_by(subject_id) %>% 
                summarise(fracture_after_index = min(condition_start_date, na.rm =  T)),
              by = "subject_id")
}

# add a column of death record date after the index date
addInDeath <- function(fractureTable){
  fractureTable %>% 
    left_join(cdm[["death"]], by = c("subject_id" = "person_id"), copy = T) %>%
    filter(death_date > index_date | is.na(death_date)) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site, index_date, after_index, observation_period_end_date, cancer_date, mbd_date, fracture_after_index, death_date)
}

# add in FOLLOWUPEND - only after the relevant columns are added
addInFollowUpEnd <- function(fractureTable){
  fractureTable %>% 
    mutate(follow_up_end = pmin(after_index, observation_period_end_date, cancer_date, mbd_date, fracture_after_index, death_date, na.rm = T)) %>%
    mutate(follow_up_time = follow_up_end-index_date) 
}

# clean out fractures based on their follow up period, used for further analysis 
nextFractureClean <- function (fractureTable){
  fractureTable %>%
    anti_join(fractureTable %>% filter(imminentFracture==1|follow_up_time <730), by = "subject_id") %>%
    group_by(subject_id) %>%
    arrange(condition_start_date, .by_group = T) %>%
    filter(condition_start_date>index_date) %>%
    select(subject_id, cohort_start_date, cohort_end_date, condition_concept_id, condition_start_date, fracture_site) %>%
    ungroup()%>%
    anti_join(fractureTable %>% 
                         mutate(within = condition_start_date<=cohort_end_date) %>%
                         group_by(subject_id) %>%
                         summarise(tot = sum(within), .groups = "drop") %>%
                         filter(tot == 0), by = "subject_id") 
}

# add in immFracture, a function that indicates which fracture is considered imminent in relation to the current index fracture
immFracture <- function (fractureTable){
  fractureTable %>%
    mutate(imminentFracture = as.integer(condition_start_date == follow_up_end))
}

# finding the common columns of target cohort and comp cohort 1 where each has at least lower_bound of covariates 
lowerBoundLasso01 <- function(subfeatures_01, lower_bound){
  subfeatures_01_0 <- subfeatures_01 %>% 
    inner_join(targetCohort[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  subfeatures_01_1 <- subfeatures_01 %>%
    inner_join(compCohort1[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  comms <- intersect(subfeatures_01_0, subfeatures_01_1)
  rm(subfeatures_01_0, subfeatures_01_1)
  subfeatures_01 <- subfeatures_01 %>%
    filter(feature %in% comms)
  return(subfeatures_01)
}

# finding the common columns of target cohort and comp cohort 1 where each has at least lower_bound of covariates 
lowerBoundLasso12 <- function(subfeatures_12, lower_bound){
  subfeatures_12_1 <- subfeatures_12 %>% 
    inner_join(compCohort1[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  subfeatures_12_2 <- subfeatures_12 %>%
    inner_join(compCohort2[[i]] %>% select(subject_id, index_date), by = c("subject_id", "index_date")) %>%
    distinct() %>%
    group_by(feature) %>%
    tally() %>% filter(n>=lower_bound) %>% 
    pull(feature)
  
  comms <- intersect(subfeatures_12_1, subfeatures_12_2)
  rm(subfeatures_12_1, subfeatures_12_2)
  subfeatures_12 <- subfeatures_12 %>%
    filter(feature %in% comms)
  return(subfeatures_12)
}

# cif
CumIncidence <- function(ftime, fstatus, group, t, strata, rho = 0, 
                         cencode = 0, subset, na.action = na.omit, level,
                         xlab = "Time", ylab = "Probability", 
                         col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  # collect data
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  # fit model and estimates at time points
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  # print result
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  #
  if(missing(level))
  { # plot cumulative incidence functions
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd      
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    # compute pointwise confidence intervals
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    # plot pointwise confidence intervals
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    # print pointwise confidence intervals
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  # return results
  invisible(out)
}
