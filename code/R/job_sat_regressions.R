
library(dplyr)
library(ExPanDaR)

soep_data <- readRDS("data/generated/soep_data.rds")
soep_data <- data.frame(soep_data)
soep_data$plh0173 <- as.numeric(soep_data$plh0173)
soep_data$plh0176 <- as.numeric(soep_data$plh0176)
soep_data$plh0178 <- as.numeric(soep_data$plh0178)

###########
# Accountant job satisfaction
###########

# Dependent variable:     Job Satisfaction
# Independent variable:   Accountant
# Control group:          All Professionals


# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  2 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  2,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample                <- sample[!(sample$pid %in% unique(sample$pid[sample$accountant==1]) & sample$accountant == 0),]



tab_accountant_job_sat <-  prepare_regression_table(
  sample,
  dvs = c("plh0173", "plh0176","plh0178"),
  idvs = list(
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd")
  
  ),
  feffects = list(c("syear"),c("syear"),c("syear")),

  format = "latex"
)


var_names_accountants_job_sat <- tibble(
  var_name = c("plh0173",
               "plh0176",
               "plh0178",
               "accountant",
               "female", 
               "age", 
               "higher_edu", 
               "pglabgro", 
               "pguebstd"),
  label = cdesc_rnames <- c(
    "Job Satisfaction",
    "Income Satisfaction",
    "Leisure Satisfaction",
    "Sample (1 = Accountant)",
    "Gender (1 = Female)",
    "Age",
    "Higher Education",
    "Gross Salary",
    "Overtime"
  )
)




###########
# Bookkeeper job satisfaction
###########

# Dependent variable:     Job Satisfaction
# Independent variable:   Bookkeeper
# Control group:          All Professionals


# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  3 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  3,]
sample$bookkeeper     <- sample$pgisco88 %in%  3433 | sample$pgisco08 %in%  3433
sample                <- sample[!(sample$pid %in% unique(sample$pid[sample$bookkeeper==1]) & sample$bookkeeper == 0),]



tab_bookkeeper_job_sat <-  prepare_regression_table(
  sample,
  dvs = c("plh0173", "plh0176","plh0178"),
  idvs = list(
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd")
    
  ),
  feffects = list(c("syear"),c("syear"),c("syear")),
  
  format = "latex"
)


var_names_bookkeeper_job_sat <- tibble(
  var_name = c("plh0173",
               "plh0176",
               "plh0178",
               "bookkeeper",
               "female", 
               "age", 
               "higher_edu", 
               "pglabgro", 
               "pguebstd"),
  label = cdesc_rnames <- c(
    "Job Satisfaction",
    "Income Satisfaction",
    "Leisure Satisfaction",
    "Sample (1 = Bookkeeper)",
    "Gender (1 = Female)",
    "Age",
    "Higher Education",
    "Gross Salary",
    "Overtime"
  )
)


###########
# Accounting Clerk job satisfaction
###########

# Dependent variable:     Job Satisfaction
# Independent variable:   Bookkeeper
# Control group:          All Professionals

# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  4 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  4,]
sample$acc_clerk      <- sample$pgisco88 %in%  4121 | sample$pgisco08 %in%  4311
sample                <- sample[!(sample$pid %in% unique(sample$pid[sample$acc_clerk==1]) & sample$acc_clerk == 0),]


tab_acc_clerk_job_sat <-  prepare_regression_table(
  sample,
  dvs = c("plh0173", "plh0176","plh0178"),
  idvs = list(
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd")
    
  ),
  feffects = list(c("syear"),c("syear"),c("syear")),
  
  format = "latex"
)


var_names_acc_clerk_job_sat <- tibble(
  var_name = c("plh0173",
               "plh0176",
               "plh0178",
               "acc_clerk",
               "female", 
               "age", 
               "higher_edu", 
               "pglabgro", 
               "pguebstd"),
  label = cdesc_rnames <- c(
    "Job Satisfaction",
    "Income Satisfaction",
    "Leisure Satisfaction",
    "Sample (1 = Accounting Clerk)",
    "Gender (1 = Female)",
    "Age",
    "Higher Education",
    "Gross Salary",
    "Overtime"
  )
)





save(
  list = c(ls(pattern = "^var_names_*"), ls(pattern = "^tab_*")),
  file = "output/results_job_sat.rda"
)



