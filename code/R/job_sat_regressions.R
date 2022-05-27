
library(dplyr)
library(ExPanDaR)

soep_data <- readRDS("data/generated/soep_data.rds")

###########
# Accountants vs. Professionals personality traits
###########

# Dependent variable:     Job satisfaction
# Independent variable:   Accountant
# Control group:          Professionals
# Fixed effects:          Survey year

# Get sample
# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  2 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  2,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample$gender_age     <- sample$female*sample$age
sample$pglabgro[sample$pglabgro==0] <- NA
sample$log_wage  <- log(sample$pglabgro)


tab_accountant_job_sat_YearFE <-  prepare_regression_table(
  sample,
  dvs = c("plh0173","plh0176","plh0178","plh0182"),
  idvs = list(
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit"),
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit"),
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit"),
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit")
  ),
  feffects = list(c("syear"),c("syear"),c("syear"),c("syear")),
  cluster = list(c("syear"),c("syear"),c("syear"),c("syear")),
  format = "latex"
)






###########
# Accountants vs. Professionals personality traits
###########

# Dependent variable:     Job satisfaction
# Independent variable:   Accountant
# Control group:          Professionals
# Fixed effects:          Person

# Get sample
# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  2 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  2,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample$gender_age     <- sample$female*sample$age
sample$pglabgro[sample$pglabgro==0] <- NA
sample$log_wage  <- log(sample$pglabgro)


tab_accountant_job_sat_PersonFE <-  prepare_regression_table(
  sample,
  dvs = c("plh0173","plh0176","plh0178","plh0182"),
  idvs = list(
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit"),
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit"),
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit"),
    c("accountant", "female", "age", "higher_edu", "log_wage", "pgtatzeit")
  ),
  feffects = list(c("pid"),c("pid"),c("pid"),c("pid")),
  cluster = list(c("pid"),c("pid"),c("pid"),c("pid")),
  format = "latex"
)




###########
# Define names and safe
###########


# Define variable names for the regression tables

var_names_accountants_sat <- tibble(
  var_name = c("plh0173",
               "plh0176",
               "plh0178",
               "plh0182",
               "accountant",
               "female", 
               "age",
               "higher_edu", 
               "log_wage", 
               "pgtatzeit"),
  label = cdesc_rnames <- c(
    "Work satisfaction",
    "Income satisfaction",
    "Leisure satisfaction",
    "Life satisfaction",
    "Sample (1 = Accountant)",
    "Gender (1 = Female)",
    "Age",
    "Higher Education (1 = Yes)",
    "log(Income)",
    "Working Time"
  )
)



save(
  list = c(ls(pattern = "^var_names_*"), ls(pattern = "^tab_*")),
  file = "output/results_satisfaction.rda"
)




