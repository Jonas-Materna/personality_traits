
library(dplyr)
library(ExPanDaR)

soep_data <- readRDS("data/generated/soep_data.rds")
soep_data <- data.frame(soep_data)

###########
# Accountant personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accountant
# Control group:          All Professionals

# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  2 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  2,]
sample$accountant     <- sample$pgisco88 %in%  2411 | sample$pgisco08 %in%  2411
sample                <- sample[!(sample$pid %in% unique(sample$pid[sample$accountant==1]) & sample$accountant == 0),]


# Take the first observation for each accountant
sample <- sample[complete.cases(sample[,c("consc", "extra", "agree", "openn", "neuro")]),] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

tab_accountant_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("accountant", "female", "age", "higher_edu", "pglabgro", "pguebstd")
  ),
  format = "latex"
)


var_names_accountants_big5 <- tibble(
  var_name = c("consc", 
               "extra", 
               "agree", 
               "openn", 
               "neuro",
               "accountant",
               "female", 
               "age", 
               "higher_edu", 
               "pglabgro", 
               "pguebstd"),
  label = cdesc_rnames <- c(
    "Conscientiousness",
    "Extraversion",
    "Agreeableness",
    "Openness",
    "Neuroticism",
    "Sample (1 = Accountant)",
    "Gender (1 = Female)",
    "Age",
    "Higher Education",
    "Gross Salary",
    "Overtime"
  )
)




###########
# Bookkeeper personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Bookkeeper
# Control group:          All Technicians

# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  3 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  3,]
sample$bookkeeper     <- sample$pgisco88 %in%  3433 | sample$pgisco08 %in%  3433
sample                <- sample[!(sample$pid %in% unique(sample$pid[sample$bookkeeper==1]) & sample$bookkeeper == 0),]


# Take the first observation for each accountant
sample <- sample[complete.cases(sample[,c("consc", "extra", "agree", "openn", "neuro")]),] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

tab_bookkeeper_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("bookkeeper", "female", "age", "higher_edu", "pglabgro", "pguebstd")
  ),
  format = "latex"
)

var_names_bookkeeper_big5 <- tibble(
  var_name = c("consc", 
               "extra", 
               "agree", 
               "openn", 
               "neuro",
               "bookkeeper",
               "female", 
               "age", 
               "higher_edu", 
               "pglabgro", 
               "pguebstd"),
  label = cdesc_rnames <- c(
    "Conscientiousness",
    "Extraversion",
    "Agreeableness",
    "Openness",
    "Neuroticism",
    "Sample (1 = Bookkeeper)",
    "Gender (1 = Female)",
    "Age",
    "Higher Education",
    "Gross Salary",
    "Overtime"
  )
)


###########
# Accounting Clerks personality traits
###########

# Dependent variable:     Personality traits
# Independent variable:   Accounting Clerks
# Control group:          All Clerks

# Get sample
sample                <- soep_data[as.numeric(substr(soep_data$pgisco88, 1, 1)) %in%  4 | as.numeric(substr(soep_data$pgisco08, 1, 1)) %in%  4,]
sample$acc_clerk      <- sample$pgisco88 %in%  4121 | sample$pgisco08 %in%  4311
sample                <- sample[!(sample$pid %in% unique(sample$pid[sample$acc_clerk==1]) & sample$acc_clerk == 0),]


# Take the first observation for each accountant
sample <- sample[complete.cases(sample[,c("consc", "extra", "agree", "openn", "neuro")]),] %>% 
  group_by(pid) %>% 
  filter(row_number()==1)

tab_acc_clerks_big5 <-  prepare_regression_table(
  sample,
  dvs = c("consc", "extra", "agree", "openn", "neuro"),
  idvs = list(
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd"),
    c("acc_clerk", "female", "age", "higher_edu", "pglabgro", "pguebstd")
  ),
  format = "latex"
)



var_names_acc_clerks_big5 <- tibble(
  var_name = c("consc", 
               "extra", 
               "agree", 
               "openn", 
               "neuro",
               "acc_clerk",
               "female", 
               "age", 
               "higher_edu", 
               "pglabgro", 
               "pguebstd"),
  label = cdesc_rnames <- c(
    "Conscientiousness",
    "Extraversion",
    "Agreeableness",
    "Openness",
    "Neuroticism",
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
  file = "output/results_big5.rda"
)


mdl <- lm(formula = neuro ~ acc_clerk, data = sample)
summary(mdl)




